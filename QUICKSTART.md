# Introduction

This is a user guide for getting started with the `contingent-claims` library.
The library is used for modelling arbitrary financial instruments, typically derivatives.
This guide is meant to help you get to grips with the library as quickly as possible; it's not meant to cover every aspect. The work is based on the papers [[1]](#1), [[2]](#2), and we recommend that you refer to these for an in-depth understanding of how it works.

# The Model

The approach taken in the papers is to model financial instruments by their cashflows. This should be familiar to anyone having taken a course in corporate finance or valuation. Let's start with an example:

![image](https://user-images.githubusercontent.com/53480858/117022215-59007080-acf8-11eb-9820-9aebf41602e1.png)

The picture above represents the cashflows of a fixed-rate bond. Or alternatively, you can think of it as a mortgage, from the point of view of the bank: there are interest payments at regular intervals (the small arrows), and a single repayment of the loan at maturity (the big arrow on the right). So how do we go about modelling this?

We use the following data type, slightly simplified from [`Claim.daml`](./daml/ContingentClaims/Claim.daml):

```Haskell
data Claim a
 = Zero
 | One a
 | Give (Claim a)
 | And with lhs: Claim a, rhs: Claim a
 | Or with lhs: Claim a, rhs: Claim a
 | Scale with k: Date -> Decimal, claim: Claim a
 | When with predicate: Date -> Bool, claim: Claim a
 | Anytime with predicate : Date -> Bool, claim: Claim a
 | Until with predicate : Date -> Bool, claim: Claim a
```

There are couple of things to consider.

First note that the constructors of this data type create a tree structure. The leaf constructors are `Zero` and `One a`, and the other constructors create branches (observe they call `Claim a` recursively). The constructors are just functions, and can be combined to produce complex cashflows. For example, to represent the above bond, we could write the following:

```Haskell
When (time == t_0) (Scale (pure coupon) (One “USD”)) `And` ...
```

Let's look at the constructors used in the above expression in more detail:

* `One "USD"` means that the acquirer of the contract receives one unit of the asset, parametrised by `a`, *immediately*. In this case we use a 3-letter ISO code to represent a currency. But you can use your own type to represent any asset.
* `Scale (pure coupon)` modifies the *magnitude* of the arrow in the diagram. So for example, in the diagram, the big arrow would have a distinct scale factor from the small arrows. Note also, that in our example the scale factor is constant : `pure coupon = const coupon`. However, it's possible to have a scale factor that varies on an unobserved value, such as a stock price, the weather, or any other measurable quantity.
* `When (time == t_0)` tells us where along the x-axis the arrow is placed. i.e. it modifies the moment the claim is acquired. The convention is that this must be the first instant that the predicate (`time == t_0` in this case) is true. In our example it is a point, but again, we could have used an expression with an unknown quantity, for example `spotPrice > pure k`, and it would trigger *the first instant* that the expression becomes true.
* `And` is used to chain multiple expressions together. Remember that in the `data` definition above, each constructor is a function: `And : Claim a -> Claim a -> Claim a`. We use the Daml backtick syntax to write `And` as an infix operator, for legibility.

Additionally we have several constructors which we've not used in this example:

* `Zero`, used to indicate an absence of obligations. While it may not make sense to create a `Zero` claim, it could, for example, result from applying a function on a tree of claims.
* `Give` would flip the direction of the arrows in our diagram. So for example, in a swap we could use `Give` to distinguishing the received/paid legs.
* `Or` is used to give the bearer the right to choose between two different claims.
* `Anytime` is like `When`, except it allows the bearer to choose (vs. no choice) acquisition in a *region* (vs. a point).
* `Until` is used to adjust the expiration (*horizon* in [[1]]) of a claim. Typically used with `Anytime` to limit aforesaid acquisition region.

The tree produced by our expression is pictured below:

![image](https://user-images.githubusercontent.com/53480858/117022528-9b29b200-acf8-11eb-9629-af2b44019261.png)

# Composition and Extensibility

Although we could model every subsequent arrow the way we did the first one, as good programmers we wish to avoid repeating ourselves. Hence we could write functions to re-use subexpressions of the tree. But which parts should we factor out? It turns out, that Finance 101 comes to the rescue again. Fixed income practitioners will typically model a fixed-rate bond as a sum of zero-coupon bonds. That's how we model them in [`FinancialClaims.daml`](./daml/ContingentClaims/FinancialClaim.daml). Below are slightly simplified versions:

```Haskell
zcb maturity principal asset =
 when (time == maturity) (scale (O.pure principal) (one asset))
```

Here we've just wrapped our expression from the previous section in a function `zcb`, that we can reuse to build the `fixed`-rate bond:

```Haskell
fixed : Decimal -> Decimal -> a -> [t] -> Claim a
fixed principal coupon asset [] = Zero
fixed principal coupon asset [maturity] = zcb maturity coupon asset `And` zcb maturity principal asset
fixed principal coupon asset (t :: ts) = zcb t coupon asset `And` fixed principal coupon asset ts
```
We define the fixed rate bond by induction, iterating over a list of dates `[t]`, and producing multiple zero-coupon bonds `zcb` combined together with `And`.
* The first definition covers the trivial case where we pass an empty list of dates.
* The second definition handles the base case, at maturity: we create both a coupon (interest) payment, and the principal payment.
* The third definition is the induction step; it peels the first element off the list, and calls itself recursively on the tail of the list, until it reaches the base case at maturity.

This re-use of code is prevalent throughout the library. It's great as it mirrors how instruments are defined in the industry. Let's look at yet another example, a fixed vs floating USD/EUR swap.

```Haskell
type Ccy = Text
usdVsEur : [Date] -> Claim Ccy
usdVsEur = fixed 100.0 0.1 "USD" `swap` floating (spot "EURUSD" * pure 100.0) (spot "EURUSD") "EUR"
```

We define it in terms of its two legs, `fixed` and `floating`, which themselves are functions. We use `swap` in infix form, and partially apply it - it takes a final `[Date]` argument which we omit, hence the resulting signature `[Date] -> Claim Ccy`.

As you can see, not only is this approach highly composable, but it also mirrors the way derivative instruments are modelled in finance.

Another major advantage of this approach is its extensibility. Unlike a traditional approach, where we might in an object-oriented language represent different instruments as classes, in the cashflow approach, we do not need to enumerate possible asset classes/instruments *a priori*. This is especially relevant in a distributed setting, where parties must execute the same code i.e. have the same `*.dar`s  on their ledger to interact. In other words, party A can issue a new instrument, or even write a new combinator function that is in a private `*.dar`, while being able to trade with party B, who has no knowledge of this new `*.dar`.

# Concerning Type Parameters

The curious reader may have noticed that the signature we gave for `data Claim` is not quite what is in the library, where we have `data Claim f t a`. In our examples, we have specialised this to `type Claim' f t a = Claim (->) Date a`. We'll briefly explain the need for this generality, and the three type parameters:

## The Effect Parameter

`f : * -> * -> *` is a binary type constructor, and in our examples we used `f t a = (->) t a`, which you should think of as `t -> a`. This allows us to express time-dependent values in expressions involving `Scale` or `When`. Although using `(->)` is great for learning the library, it has two main limitations. First, it is not serializable, which means you can't store it in a contract on-ledger; and second, because functions in Daml are pure, it means we can't really have unknown (future) quantities. So we *could* write something like:

```Haskell
price : Date -> Decimal
price t = case toGregorian t of
   (2021, May, 4) -> 121.50
   (2021, May, 5) -> 121.00
   (2021, May, 6) -> 123.35
   anotherDate -> error "No stock price for this date"
```

to use as an `(->) Date Decimal`. But as you can see, this isn't practical: we'd need to enumerate all future stock prices.

The solution is to read this data from a contract on the ledger. In Daml, the effect of reading (or writing) from the ledger is encapsulated by the `data Update` type. So what we would want is something like `f = (-> Update _)`. Unfortunately this still doesn't solve the serializability issue.

To work around this, the typeclass [Observable](./daml/ContingentClaims/Observable.daml) abstracts these details away. We provide a concrete implementation that is both serializable and effectful, in [`Serializable/Claim.daml`](./daml/ContingentClaims/Serializable/Claim.daml). It uses `Observation` as a concrete, serializable implementation of `Observable`.

## The Time Parameter

`t` is used to represent the input argument to `f`, and above we used `Date` for this purpose. The reason this has been left parametrised is to be able to distinguish different calendar and day count conventions at the type level. This is quite a technical topic, but it suffices to know that for financial calculations, interest is not always accrued the same way, nor is settlement possible every day, as this depends on local jurisdictions or market conventions. Having different types makes this explicit at the instrument level.

One use of this is having instruments 'templates' expressed with time as an ordinal value, representing e.g. days from issue, which can then be re-used to list at different dates. Think for example, of listed futures or options which are listed at regular intervals.

## The Asset Parameter

`a` as we already explained, is the type used to represent assets in our program. Keeping this generic means the library can be used with any asset representation. For example, you could use that in [lib-finance](https://github.com/digital-asset/lib-finance), but are not forced to do so.

# Lifecycling

So far we've learned how to model arbitrary financial instruments by representing them as trees of cashflows. We've seen that these trees can be constructed using the type constructors of `data Claim`, and that they can be factored into more complex building blocks using function composition. But now that we have these trees, what can we do with them?

The original paper [[1]](#1) focuses on using these trees for valuing the instruments they represent, i.e. finding the 'fair price' that one should pay for these cashflows. Instead, we'll focus here on a different use-case: the lifecycling (aka safekeeping, processing corporate actions) of these instruments.

Let's go back to our fixed-rate bond example, above. We want to process the coupon payments. There is a function in [`Lifecycle.daml`](./daml/ContingentClaims/Lifecycle.daml) for doing just this:

```Haskell
type C a = Claim Observation Date a

data Result a = Result with
  remaining : C a
  pending : [(Decimal, a)]

lifecycle : (Eq a, CanAbort m)
  => (Text -> Date -> m Decimal)
  -> C a
  -> Date -> m (Result a)
```

This may look daunting, but let's look at an example in [`Test/FinancialContract.daml`](test/daml/Test/FinancialContract.daml) to see this in action:

```Haskell
do t <- toDateUTC <$> getTime
   let getSpotRate isin t = do
         (_, Quote{close}) <- fetchByKey (isin, t, bearer)
         pure close
   lifecycleResult <- Lifecycle.lifecycle getSpotRate claims t
```

The first argument to lifecycle, `getSpotRate`, is a function taking an ISIN (security) code, and today's date. All this does is fetch a contract from the ledger that is keyed by these two values, and extract the `close`ing price of the security.

The last two arguments are simply the claims we wish to process, and today's date, evaluated using `getTime`.

The return value, `lifecycleResult`, will contain both the remaining tree after lifecycling, and any assets that need to be settled. In our running bond example, we would extract the `coupon` from the first payment, and return it, along with the rest of the tree, after that branch has been pruned (depicted greyed-out below):

![image](https://user-images.githubusercontent.com/53480858/117022642-af6daf00-acf8-11eb-8115-a495fa78a067.png)

You may wonder why we've separated the settlement procedure from the lifecycling function. The reason is that we can't assume that settlement will happen on-chain; if it does, that is great, as we can embed this call into a template choice, and lifecycle & settle atomically. However, in the case where settlement must happen off-chain, there's no way to to do this in one step. This design supports both choices.

# References

<a id="1">[1]</a>
Jones, S. Peyton, Jean-Marc Eber, and Julian Seward.
"Composing contracts: an adventure in financial engineering."
ACM SIG-PLAN Notices 35.9 (2000): 280-292.

<a id="2">[2]</a>
Jones, SL Peyton, and J. M. Eber.
"How to write a financial contract",
volume "Fun Of Programming" of "Cornerstones of Computing." (2005).



# Changelog

This document tracks pending changes to packages. It is facilitating the write-up of release notes.

## Stable Packages

| Package                                    | Released version   | Target version |
|--------------------------------------------|--------------------|----------------|
| ContingentClaims.Core                      | 2.0.1              |                |
| ContingentClaims.Lifecycle                 | 2.0.1              |                |
| Daml.Finance.Account                       | 3.0.0              |                |
| Daml.Finance.Claims                        | 2.1.0              |                |
| Daml.Finance.Data                          | 3.0.0              |                |
| Daml.Finance.Holding                       | 3.0.0              |                |
| Daml.Finance.Instrument.Bond               | 2.0.0              |                |
| Daml.Finance.Instrument.Generic            | 3.0.0              |                |
| Daml.Finance.Instrument.Token              | 3.0.0              |                |
| Daml.Finance.Interface.Account             | 3.0.0              |                |
| Daml.Finance.Interface.Claims              | 3.0.0              |                |
| Daml.Finance.Interface.Data                | 3.1.0              |                |
| Daml.Finance.Interface.Holding             | 3.0.0              |                |
| Daml.Finance.Interface.Instrument.Base     | 3.0.0              |                |
| Daml.Finance.Interface.Instrument.Bond     | 2.0.0              |                |
| Daml.Finance.Interface.Instrument.Generic  | 3.0.0              |                |
| Daml.Finance.Interface.Instrument.Token    | 3.0.0              |                |
| Daml.Finance.Interface.Instrument.Types    | 1.0.0              |                |
| Daml.Finance.Interface.Lifecycle           | 3.0.0              |                |
| Daml.Finance.Interface.Settlement          | 3.0.0              |                |
| Daml.Finance.Interface.Types.Common        | 2.0.0              |                |
| Daml.Finance.Interface.Types.Date          | 2.1.0              |                |
| Daml.Finance.Interface.Util                | 2.1.0              |                |
| Daml.Finance.Lifecycle                     | 3.0.0              |                |
| Daml.Finance.Settlement                    | 3.0.0              |                |
| Daml.Finance.Util                          | 3.1.0              |                |

## Early Access Packages

| Package                                             | Released version   | Target version |
|-----------------------------------------------------|--------------------|----------------|
| ContingentClaims.Valuation                          | 0.2.2              |                |
| Daml.Finance.Instrument.Equity                      | 0.4.0              |                |
| Daml.Finance.Instrument.Option                      | 0.3.0              |                |
| Daml.Finance.Instrument.StructuredProduct           | 0.1.0              |                |
| Daml.Finance.Instrument.Swap                        | 0.4.0              |                |
| Daml.Finance.Interface.Instrument.Equity            | 0.4.0              |                |
| Daml.Finance.Interface.Instrument.Option            | 0.3.0              |                |
| Daml.Finance.Interface.Instrument.StructuredProduct | 0.1.0              |                |
| Daml.Finance.Interface.Instrument.Swap              | 0.4.0              |                |

## Pending changes

### ContingentClaims.Core

### ContingentClaims.Lifecycle

### ContingentClaims.Valuation

### Daml.Finance.Account

Dependency update (patch).

### Daml.Finance.Claims

Dependency update (patch).

### Daml.Finance.Data

Dependency update (patch).

### Daml.Finance.Holding

Dependency update (patch).

### Daml.Finance.Instrument.Bond

Dependency update (patch).

### Daml.Finance.Instrument.Equity

Dependency update (patch).

### Daml.Finance.Instrument.Generic

Dependency update (patch).

### Daml.Finance.Instrument.Option

Dependency update (patch).

### Daml.Finance.Instrument.StructuredProduct

Dependency update (patch).
Add new AutoCallable instrument (minor).

### Daml.Finance.Instrument.Swap

Dependency update (patch).

### Daml.Finance.Instrument.Token

Dependency update (patch).

### Daml.Finance.Interface.Account

### Daml.Finance.Interface.Claims

### Daml.Finance.Interface.Data

Dependency update (patch).

### Daml.Finance.Interface.Holding

### Daml.Finance.Interface.Instrument.Base

### Daml.Finance.Interface.Instrument.Bond

### Daml.Finance.Interface.Instrument.Equity

Dependency update (patch).

### Daml.Finance.Interface.Instrument.Generic

### Daml.Finance.Interface.Instrument.Option

Dependency update (patch).

### Daml.Finance.Interface.Instrument.StructuredProduct

Add new AutoCallable instrument (minor).

### Daml.Finance.Interface.Instrument.Swap

### Daml.Finance.Interface.Instrument.Token

### Daml.Finance.Interface.Instrument.Types

### Daml.Finance.Interface.Lifecycle

Update a comment (this should generally not trigger a version bump, but packell is unhappy as I effectively removed a comment line, which forces the package bump).

### Daml.Finance.Interface.Settlement

### Daml.Finance.Interface.Types.Common

### Daml.Finance.Interface.Types.Date

### Daml.Finance.Interface.Util

### Daml.Finance.Lifecycle

Dependency update (patch).

### Daml.Finance.Settlement

Dependency update (patch).

### Daml.Finance.Util

Only release a Semaphore lock if the provided context is recognized (patch).

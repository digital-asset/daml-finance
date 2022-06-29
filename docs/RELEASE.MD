# Release

The purpose of this document is to describe the procedure to release this library.

## Release steps

1. Create a new local Git branch.
2. In [daml.yaml](../daml.yaml), update the `version` value as required :
   - To check what `versions` already exist, check the existing tags [here](https://github.com/digital-asset/contingent-claims/tags).
   - Note - do not add a `v` in front of the `version` in [daml.yaml](../daml.yaml). The build script handles the naming conventions of the release, the tag and the release artifacts.
3. Run `daml build` :
   - This command validates the specified `version`;
   - A successful build means the `version` specified is valid.
4. Push your branch to GitHub.
5. Create a pull request to merge your branch to `master`.
6. Get approval and merge this branch.
7. When the branch gets merged to `master`, a CI job gets triggered which will build the project and then perform the release:
   - If building the project fails, the release step will not get triggered;
   - If the build is successful then :
     - If the `version` already exists, the release will fail;
     - If the `version` doesn't exist, it will create :
       - a new release in GitHub releases;
       - generated release notes;
       - a new tag in GitHub.
8. A successful release will be listed [here](https://github.com/digital-asset/contingent-claims/releases).

## Official release versioning

This project will follow Semantic Versioning :

```
Given a version number `MAJOR.MINOR.PATCH`, increment the:

1. MAJOR version when you make incompatible API changes,
2. MINOR version when you add functionality in a backwards compatible manner, and
3. PATCH version when you make backwards compatible bug fixes.
```

See [here](https://semver.org/) for further information.


### Pre-release versioning

Due to the underying Haskell tooling which Daml is built upon, versioning is limited to the following regex:

```
^(0|[1-9][0-9]*)(\.(0|[1-9][0-9]*))*$
```

For pre-release versioning we cannot use `-ALPHA`, `-SNAPSHOT`, `-RC1`, etc. For example:

```
1.0.0-alpha < 1.0.0-alpha.1 < 1.0.0-alpha.beta < 1.0.0-beta < 1.0.0-beta.2 < 1.0.0-beta.11 < 1.0.0-rc.1 < 1.0.0.
```

To overcome this limitation, we will use dates to represent pre-release canditates. For example:

```
1.0.0.20220627.1 < 1.0.0.20220627.2 < 1.0.0.20220628.1 < 1.0.0
```

Therefore, any release version specified outside of the Official release versioning will be marked as 'Pre-Release' in GitHub (for example, any version outside of `MAJOR.MINOR.PATCH`).

# Release

The purpose of this document is to describe the procedure to release this library.

## Release steps

1. The target branch (e.g. `main`) must contain the changes you want to release along with the
   version to release in the required packages `daml.yaml` file.
2. Create a new branch either directly on GitHub or locally from the target branch (i.e., `main`),
   with the branch name beginning with `release`. If its a local branch, push to remote.
3. The CI will pick up the release branch and:
   - If building the project fails, the release workflow will not get triggered.
   - If the build is successful, the release workflow will execute for each package:
     - If the `package_name\version` already exists as a git tag, a release will not be created for
       that package.
     - If the `package_name\version` doesn't exist as a git tag, it will create:
       - a new release in GitHub releases
       - a new tag in GitHub
   - You can find the `daml-finance` Circle CI's pipeline of executing and running jobs
     [here](https://app.circleci.com/pipelines/github/digital-asset/daml-finance).
4. Successful releases will be listed
   [here](https://github.com/digital-asset/daml-finance/releases).

## Official release versioning

This project will follow Semantic Versioning:

```{}
Given a version number `MAJOR.MINOR.PATCH`, increment the:

1. MAJOR version when you make incompatible API changes,
2. MINOR version when you add functionality in a backwards compatible manner, and
3. PATCH version when you make backwards compatible bug fixes.
```

See [here](https://semver.org/) for further information.

### Pre-release versioning

Due to the underying Haskell tooling which Daml is built upon, versioning is limited to the
following regex:

```{}
^(0|[1-9][0-9]*)(\.(0|[1-9][0-9]*))*$
```

For pre-release versioning we cannot use `-ALPHA`, `-SNAPSHOT`, `-RC1`, etc. For example:

```{}
1.0.0-alpha < 1.0.0-alpha.1 < 1.0.0-alpha.beta < 1.0.0-beta < 1.0.0-beta.2 < 1.0.0-beta.11 < 1.0.0-rc.1 < 1.0.0
```

To overcome this limitation, we will use `.99.` as a placeholder to signify a snapshot along with
the date the snapshot was generated.

The pre-release snapshot version for a MAJOR version has the `.99.` placeholder set as the MINOR
version. For example, the pre-release snapshot for version `2.0.0` is :

```{}
1.99.0.20230206.1 < 1.99.0.20230206.2 < 1.99.0.20230207.1 < 1.99.1.20230207.1 < 2.0.0
```

The pre-release snapshot version for a MINOR version has the `.99.` placeholder in the PATCH
version. For example, the pre-release snapshot for version `2.1.0` is :

```{}
2.0.99.20230206.1 < 2.0.99.20230206.2 < 2.0.99.20230207.1 < 2.1.0
```

The pre-release snapshot version for a PATCH version has the `.99.` placeholder after the PATCH
version. For example, the pre-release snapshot for version `2.1.1` is :

```{}
2.1.0.99.20230206.1 < 2.1.0.99.20230206.2 < 2.1.0.99.20230207.1 < 2.1.1
```

Therefore, any release version specified outside of the Official release versioning will be marked
as 'Pre-Release' in GitHub (for example, any version outside of `MAJOR.MINOR.PATCH`).

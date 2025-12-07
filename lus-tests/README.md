# Lus tests

One of Lus' design goals is to be thoroughly tested, both for stability and performance. To this end, there is a suite of tests that are run as part of the build process. The tests are divided into four harnesses:

- The first harness (H1) covers syntax, parser, and standard library logic.
- The second harness (H2) tests the embeddability of the language via the C API.
- The third harness (H3) runs the runtime within a container to test for OS-specific issues.
- The fourth harness (H4) runs various benchmarks to test for performance regressions.

H1, H2, and H3 are blocking; if they fail, the build will not pass and an unstable build won't be produced. H4 is conditional, where failure blocks stable releases but may not block unstable releases depending on the severity of the failure.

H4 scores each test and ranks them as "critical", "bad", "acceptable", and "good". The ranges for each are test-specific. If _any_ test are scored critical, the build will fail. If a test is scored bad, the build will fail if the build type is Standard, but pass if the build type is Unstable or if we're releasing a security hotfix. If a test is scored acceptable or good, the build will pass; _good_ is an arbitrary threshold that we _wish_ to attain.

**It is recommended to run the tests locally before submitting _any_ pull requests.** This saves precious GitHub Actions minutes, allows for faster iteration on your end, and reduces PR clutter. It is quite easy to run the tests locally, so even if it's boring, please do it.

Each test suite can be ran on either your host operating system or within a container. It is definitely safer and more consistent (and good practice) to run the tests within a container, but H1 and H2 can be reliably ran on your host operating system without too much risk. An additional benefit of running the tests in a container (using our method below) is you will also subject Lus to additional memory sanitization tests, which Meson does not do.

## Running the tests locally

### Testing on your host operating system

Use the `meson test` command, which will run ALL tests on your host operating system. You _probably_ don't want to do all tests there (especially H3), so you should probably pick and choose which tests you want to run, as such:

```sh
# Test a specific suite, e.g. H1.
meson test --suite h1

# Test a specific test.
meson test h1/optchain
```

You can list all available tests with `meson test --list`.

### Testing within a container

The best way to run the tests in a container is to execute the `sanitize` CI workflow locally. First, make sure to have these programs installed on your computer:

- [Docker](https://www.docker.com/) (macOS, Windows) or [Podman](https://podman.io/) (Linux)
- [`act`](https://nektosact.com/)

Then, run the tests while in the root directory of the repository:

```sh
act -j sanitize
```

> [!NOTE]
> If `act` asks which kind of image you want to install, choose the medium one.

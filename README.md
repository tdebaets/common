Common Code Utilities
=====================

Common Code Utilities is a collection of source code, scripts, and various other utilities that are shared among my other projects. The purpose of this repository is to be included in my other repositories as a Git submodule. That way, those other repositories have direct access to all files in this repository. Common Code Utilities is not meant to be used as a standalone project. However, for convenience sake, it can be compiled on its own, e.g. to validate code changes.

An example of files in this repository is the source code of various Delphi components. Some of these components are third-party while others have been written from scratch. Another example is shared Git/build scripts.

-- Tim De Baets (tdebaets)

Obtaining the source code
-------------------------

First make sure that you have a recent version of the [Git client](https://git-scm.com/) (`git`) installed. Then open a Windows command prompt window (note that Git Bash isn't supported). In the command prompt, run these commands:
```
> git clone https://github.com/tdebaets/common.git common
> cd common
```

Finally, run the `postclone.bat` script. This will take care of further setting up the repository, installing Git hooks, creating output directories etc.:
```
> postclone.bat
```

To keep your repository up-to-date, run the `update.bat` script. This script essentially runs a `git pull` but also performs some basic checks before pulling. For repositories containing submodules, it also runs a `git submodule update` after the pull to update the submodules as well.

If you want to contribute to this project, don't clone its main repository, but create your own fork first and clone that fork instead. Then commit/push your work on a topic branch and submit a pull request. See [CONTRIBUTING.md](CONTRIBUTING.md) for details.

Building
--------

A large portion of Common Code Utilities consists of Borland Delphi 4 source code. This means that in order to build this project, you'll need to have Borland Delphi 4 installed and properly set up. See the [generic instructions for building Delphi projects](Delphi/Building.md).

License
-------

Common Code Utilities by Tim De Baets is Copyright © 2016-2017 Tim De Baets. It is licensed under the Apache License version 2.0, with the exception of some individual third-party files. See [LICENSE](LICENSE) for details.

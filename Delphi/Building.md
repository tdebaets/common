
Common Code Utilities by Tim De Baets  
Copyright © 2016 Tim De Baets. All rights reserved.  
For conditions of distribution and use, see [LICENSE](../LICENSE).  

Installing Borland Delphi
-------------------------

All of my Delphi projects are written for Borland Delphi 4 with all four update packs applied. Other versions of Delphi may or may not work but are unsupported. I don't have the intention or commitment to make my projects work with other Delphi versions, but pull requests to add such compatibility are still welcome of course.

If you don't have access to Delphi 4 (which is admittedly a *very* old version), please don't hesitate to send me an e-mail and we'll work something out.

Building
--------

To build the project from the command line, open a Windows command prompt in the repository directory (note that Git Bash isn't supported), run the `compile.bat` script and follow the instructions.

If you also want to compile the project from within Delphi, some additional steps are required. These are outlined in the next sections.

Configuring the Library Path
----------------------------

Launch Delphi, go to `Tools` - `Environment Options` - `Library`, and click the `...` button for `Library Path`. Then edit your library path until it matches the following. Note that the relative paths containing `common` should be absolute paths in reality:
```
common\Delphi\LibFixed
$(DELPHI)\Lib
common\Delphi\LibUser
$(DELPHI)\Bin
$(DELPHI)\Imports
common\Delphi\Imports
common\Delphi\LibUser\Virtual Treeview\Source
common\Delphi\LibUser\Virtual Treeview\Design
```

Package Installation
--------------------

If you also want to view or edit the project's forms, you need to install some additional design packages so that the Delphi IDE recognizes the third-party components used in these forms. Otherwise, if you just want to edit the code, you can skip this step and choose `Cancel` when the Delphi IDE tells you that a class can't be found.

First, make sure that you have built the `common` repository successfully. This is required to install the `tdebaets_comps.bpl` package. If you cloned a repository that includes `common` as a submodule, open a Windows command prompt in the repository directory and run these commands:
```
> cd common
> compile.bat
```

However, if you also intend to make changes to files in the `common` repository itself, you should create a separate clone of that repository and build that clone first.

Successful compilation of `common` should lead to the creation of the `tdebaets_comps.bpl` file in `Delphi\LibUser`, which should be added as a design package in the Delphi IDE (`Component` - `Install Packages...`).

Textual Form Layout Files
-------------------------

Delphi 4 stores form layouts in binary files (`.dfm`) which of course aren't very suited for version control. Therefore, for each such `.dfm` file, a textual mirror is also added to the repository and updated whenever the corresponding binary file is changed. For example, for a binary form layout file called `Main.dfm`, there will also be a textual mirror called `Main.txt`.

To let Delphi automatically create textual mirrors of form layout files, you can install the [GExperts](http://www.gexperts.org/) IDE add-in. This is a requirement if you intend to make any changes to the project's forms.

GExperts isn't being actively developed anymore for older versions of Delphi, but fortunately you can still download version 1.01 for Delphi 4 [here](http://www.gexperts.org/download/#GX101) (`GX4-101.exe`). Follow the installer instructions, launch Delphi again when the installation is finished, and go to `GExperts` - `GExperts Configuration` - `IDE`. Make sure that `Save DFMs as TXT` is ticked.

Notes
-----

- There seems to be a bug in Delphi 4 where the IDE generates an access violation in module `dcc40.dll` on the first compile (incremental, `Ctrl+F9`) after opening a project. It seems to depend on which project is compiled as not all ones are affected. The workaround is to simply make sure that the first compilation after opening a project is a *full* rebuild (`Project` - `Build <projectname>` in the IDE). All subsequent incremental compilations should then work fine. To avoid being bitten by this bug, it's a good idea to always do a full rebuild immediately after opening a project.

- Most of my Delphi projects support the `Debug` conditional compilation define. Compiling with this define includes additional debug output in the build, usually in the form of extra calls to the `OutputDebugString` API function. When the project is run under the Delphi debugger, you can capture this output using the `Send OutputDebugString To GExperts` feature of GExperts. Outside of Delphi, you can use [DebugView](https://technet.microsoft.com/en-us/sysinternals/debugview.aspx) tool.

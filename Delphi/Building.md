
Common Code Utilities by Tim De Baets  
Copyright © 2016-2025 Tim De Baets. All rights reserved.  
For conditions of distribution and use, see [LICENSE](../LICENSE).  

Installing Borland Delphi
-------------------------

All of my Delphi projects are written for Borland Delphi 4 with all four update packs applied. Other versions of Delphi may or may not work but are unsupported. I don't have the intention or commitment to make my projects work with other Delphi versions, but pull requests to add such compatibility are always welcome.

If you don't have access to Delphi 4 (which I admit is a *very* old version of Delphi), please don't hesitate to send me an e-mail and we'll work something out.

Building
--------

To build the project from the command line, open a Windows command prompt in the repository directory (note that Git Bash isn't supported), run the `compile.bat` script, and follow the instructions.

If you also want to compile the project from within the Delphi IDE, some additional steps are required. These are outlined in the next sections.

Configuring the library path
----------------------------

Launch Delphi, go to `Tools` - `Environment Options` - `Library`, and click the `...` button for `Library Path`. Then edit your library path until it matches the following list. `COMMONPATH` should be replaced by the full path to the `common` repository or submodule.
```
COMMONPATH\Delphi\LibFixed
$(DELPHI)\Lib
COMMONPATH\Delphi\LibUser
$(DELPHI)\Bin
$(DELPHI)\Imports
COMMONPATH\Delphi\Imports
COMMONPATH\Delphi\LibUser\Virtual Treeview\Source
COMMONPATH\Delphi\LibUser\Virtual Treeview\Design
COMMONPATH\Delphi\LibUser\TntUnicodeControls
COMMONPATH\Delphi\LibUser\TntUnicodeControls\Packages
COMMONPATH\Delphi\LibUser\TntUnicodeControls\Design
COMMONPATH\Delphi\LibUser\FastMM4
```

Package installation
--------------------

If you also want to view or edit the project's forms, you need to install an additional design package so that the Delphi IDE recognizes the third-party components used on these forms. Otherwise, if you just want to edit the code, you can skip this step and choose `Cancel` when the Delphi IDE tells you that a class can't be found.

First, make sure that you have built the `common` repository successfully. This is required to install the `tdebaets_comps.bpl` package. If you cloned a repository that includes `common` as a submodule, open a Windows command prompt in the repository directory, run these commands, and follow the instructions:
```
> cd common
> compile.bat
```

However, if you intend to also make changes to files in the `common` repository itself, you should create a separate clone of that repository and build that clone instead.

Successful compilation of `common` should result in the creation of the following files in `COMMONPATH\Output`. Individually add each file as a design package in the Delphi IDE (`Component` - `Install Packages...`), in the order in which they are listed:

- `tdebaets_comps.bpl`
- `TntUnicodeVcl_R40.bpl`
- `tdebaets_comps_unicode.bpl`

Textual form layout files
-------------------------

Delphi 4 stores form layouts in binary files (`.dfm`) which of course aren't very suited for version control. Therefore, for each such `.dfm` file, a textual mirror file is also added to the repository and updated whenever the corresponding binary file is changed. For example, for a binary form layout file called `Main.dfm`, there will also be a textual mirror called `Main.txt`.

To let Delphi automatically create textual mirrors of form layout files, you can install the [GExperts](http://www.gexperts.org/) IDE add-in. This is a requirement if you intend to make any changes to the project's forms.

GExperts isn't being actively developed anymore for older versions of Delphi, but fortunately you can still download version 1.01 for Delphi 4 [here](http://www.gexperts.org/download/#GX101) (`GX4-101.exe`). Follow the installer instructions and launch Delphi when the installation is finished. You should now see a new `GExperts` entry in the main menu at the top. Click it, and go to `GExperts Configuration` - `IDE`. Untick `Disable all IDE enhancements`, and tick `Save DFMs as TXT`.

Optionally, after installing GExperts, you may want to replace it with the [GExperts-unofficial](https://github.com/tdebaets/gexperts-unofficial) fork. This fork provides additional Delphi enhancements that the official version doesn't have. See the GExperts-unofficial [README.md](https://github.com/tdebaets/gexperts-unofficial/blob/master/README.md) for more details and further instructions.

Notes
-----

- The Delphi IDE doesn't work well if it's being run without administrator rights. So if you have User Account Control (UAC) turned on, it's recommended to always run Delphi as administrator. The easiest way to do so, is to mark the IDE's executable as requiring administrator rights. In Windows Explorer, head to Delphi's installation directory. In the `Bin` subdirectory, right-click `delphi32.exe` and select `Properties`. Go to the `Compatibility` tab and tick `Run this program as an administrator`. Now Windows will show an UAC prompt each time you launch the Delphi IDE.

- Most of my Delphi projects support the `Debug` conditional compilation define. Compiling with this define includes additional debugging code in the build, which are usually extra calls to the `OutputDebugString` API function. When the project is run under the Delphi debugger, you can capture the debug output using the `Send OutputDebugString To GExperts` feature of GExperts. Outside of Delphi, you can use the [DebugView](https://technet.microsoft.com/en-us/sysinternals/debugview.aspx) tool.

    > **Note:** the `Send OutputDebugString To GExperts` feature doesn't work with the official version of GExperts for Delphi 4. To be able to use it, you'll need the [GExperts-unofficial](https://github.com/tdebaets/gexperts-unofficial) fork.

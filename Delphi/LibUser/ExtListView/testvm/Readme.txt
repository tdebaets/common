*************************************************************
Note:  This version of TestVM has been modified by Brad
  Stowers for compatiblity with TExtListView v3.00, and is
  now included in the TExtListView distribution by
  permission of John Newlin.  This work still belongs solely
  to John Newlin, and he should be thanked profusely for
  providing it.  I just wish I would have had the time to
  do a demo for virtual mode that was half as nice as this.

  Disregard the section below instructing you to add two
  methods (IsSelected and ItemIndex) to TExtListView.  They
  are not needed for this modified version.

                                           --Brad Stowers
*************************************************************

Included files:

  TESTVM.DPR
  TESTVM.RES
  MAIN.PAS
  MAIN.DFM
  FILELISTING.PAS
  MASKSEARCH.PAS
  EXTLISTVIEW.PAS  {modified}
  EXTLISTV.ZIP
  README.TXT

This package is a Delphi 3.0 demonstration of how to use
the windows ListView control in the virtual mode.  The
virtual mode makes displaying long lists ultra-fast but
requires a bit more programming work.

Using the ListView virtual mode in Delphi would not be
possible if it weren't for Brad Stowers' (thanks Brad!)
TExtListView component because Borland didn't implement
the feature in their VCL TListView component.  Brad's
file is EXTLISTV.ZIP and contains two nifty demos as
well as Microsoft documentation of the virtual mode.

I chose to build an Explorer-like app because I found that
in spite of the utility of TListView features, displaying
a list of 1000 or more files was incredibly sloooow.  But
using Brad's implementation of the virtual mode I can
display the list of some 1300 files in my WINDOWS/SYSTEM
directory in about 1/2 second.

When you examine the details of the virtual mode, you will
see that the ListView control does not maintain any
information in the Items property.  This means that you
have to manage the storage and sorting of the list yourself.
I've done this through the FileListing unit.  As I
developed this demo I found that accessing the Windows
system image list to retrieve file icons and type strings
could really slow things down.  After some tinkering with
the tradeoffs between the OnODCacheHint and OnODGetItemInfo
events, I got the speed I needed.

If you already have the TExtListView component, be sure
to add the two methods that I added.  Just search for
the text below:

  {************ Added by John Newlin 12/20/97 *************}
  function IsSelected(index : integer) : boolean;
  function ItemIndex : integer;
  {********************************************************}

I used the Win 3.1 directory and drive combo controls
because everyone with Delphi 3.0 has them. Adding a
directory tree component to the the FileList
implementation of TExtListView would give you a pretty
good clone of Windows Explorer.  I've done that, but
I'm not quite ready to distribute the tree component.

The FileListing object uses the inherited TList sort
method because it was easy to implement.  Their are
undoubtedly faster, more efficient ways to sort the
list.

What needs to be done next is to turn this demo into
a file listing component.  If you do, please send me
a copy.

My code is not well commented, sorry about that, but is
straightforward enough to be useful to anyone interested
in Brad's fine work.

The MASKSEARCH unit provides a means to use a semi-colon
delimited string as a filter for file name retrieval. It
is freeware from Markus Stephany.

You can use my demonstration code in any manner you desire,
as long as you don't sell any portion of it in source or
compiled form.  Please respect the copyright of Brad Stowers ExtListView
code.

John Newlin
jnewlin@new-ware.com
December 20, 1997 (Merry Xmas!)

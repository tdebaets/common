{
Unit: aoMisc.PAS<P>
Version: 2.00<P>
This unit contains several miscellaneous objects and functions that have
a variety of uses.<P><P>

The Debugger application was designed in Delphi 4. It will require the
following 3rd party packages to compile:<P>
RXLib v2.60 http://www.rxlib.com
<P><P>

INSTALLATION<P>
For Delphi 1 and Delphi 2, simply add the file AOMISC.PAS to your
component library.<P>
For Delphi 3, 4, and 5 the recommended method of installation is
to load the AOMISCzt.DPK file, compile it, and install it. 'z' is the
version of Delphi (3, 4, or 5) while 't' is the type - R for run-time
only package and D for design-time only package.<P><P>

Contact Information:
Author: Jason Swager
Email: jswager@alohaoi.com
Web: http://www.alohaoi.com
<P>}
unit aoMisc;

{$F+} {Force far calls for unit intialization and exit code.}

interface

{Delphi 4 and Delphi 5 have different data types in calls to the WinAPI.}
{$IFDEF VER120}
  {$DEFINE HIGHDELPHI}
{$ENDIF}
{$IFDEF VER130}
  {$DEFINE HIGHDELPHI}
{$ENDIF}

uses
  SysUtils, Messages,
  {$IFDEF WIN32}
  Registry, Windows,
  {$ELSE}
  Ver, WinTypes, WinProcs,
  {$ENDIF}
  Classes, Graphics;

type
  {$IFNDEF WIN32}
  {Structure that contains the information that will be passed between
  systems. This needs to be defined for 16bit application; it has already
  been defined in the Win32 API.}
  TCopyDataStruct = packed record
    dwData: LongInt;
    cbData: LongInt;
    lpData: Pointer;
  end;
  {Pointer to a TCopyDataStruct.}
  PCopyDataStruct = ^TCopyDataStruct;
  {$ENDIF}

  {Caption name type used to define the window names in the TMiniWin object.}
  TDataCaption = array[0..128] of char;

  {Forwards.}
  TCopyData = class;
  TMiniWin  = class;

  {Exception used by the TFileArchive or TArchiveEntry objects.}
  EArchiveError = class(Exception);
  {Event type used to indicate total size or progress of a copy operation.}
  TSizedNotify = procedure(Sender: TObject; Size: LongInt) of object;
  {Event type used to indicate progress during an archive operation.}
  TArchiveFileEvent  = procedure(Sender: TObject;
                                 Bytes, Size: LongInt;
                                 FileNumber: Integer) of object;
  {Event used to notify of file or directory deletion. Disabling Go will
  prevent the deletion of the object; the default value of Go is True.}
  TDeletionNotify    = procedure(ObjectName: String; var Go: Boolean) of object;
  {Event type used to indicate incoming data in the TCopyData component. The
  return value of the function will be passed back to the TCopyData that sent
  the information.}
  TIncomingDataEvent = function(Sender: TCopyData; Data: LongInt; DataPointer: Pointer;
                                DataSize: LongInt): LongInt of object;
  {Event triggered to handle messages in the TMiniWin object.}
  TMiniEvent         = function(Sender: TMiniWin; Msg, WParam, LParam: LongInt): LongInt of object;
  {Event type used when data is requested from a TCopyData object.}
  TRequestDataEvent  = function(Sender: TCopyData; Request: Word): LongInt of object;

  {This object represents a single file within the TFileArchive object. The
  ArchiveName, Size, and DateTime properties are stored in the archive. The
  Extract property defaults to True. ExtractDir defaults to the location where
  the TFileArchive is extracted. ExtractName defaults to ArchiveName.
  SourceFile is only valid during the building operation - not upon extraction.}
  TArchiveEntry = class(TObject)
    private
      FArchiveName: String;
      FDateTime   : TDateTime;
      FExtract    : Boolean;
      FExtractDir : String;
      FExtractName: String;
      FOffset     : LongInt;
      FSize       : LongInt;
      FSourceFile : String;
    protected
      procedure SetExtractDir(Value: String); virtual;
    public
      constructor Create; virtual;
      {The name of the file as it exists within the archive. When are archive
      is opened, the ExtractName property will be set equal to this property.}
      property ArchiveName: String read FArchiveName write FArchiveName;
      {Time stamp of the SourceFile and the time stamp that will be applied
      to the file when it is extracted.}
      property DateTime: TDateTime read FDateTime write FDateTime;
      {Flag indicating that the file should be extracted on the next
      <T TFileArchive.Extract>Extract</T> method call. This property
      defaults to False after TFileArchive creation and after an archive had
      been opened.}
      property Extract: Boolean read FExtract write FExtract;
      {Directory that the file should be extracted to. This overrides the
      Dir parameter used in the <T TFileArchive.Extract>Extract</T> method
      call.}
      property ExtractDir: String read FExtractDir write SetExtractDir;
      {File name that the file should be extracted to. When the archive is first
      opened, this property is set to the ArchiveName value.}
      property ExtractName: String read FExtractName write FExtractName;
      {Offset of the start of the file within the archive.}
      property Offset: LongInt read FOffset write FOffset;
      {Size of the SourceFile or the size of the file extracted.}
      property Size: LongInt read FSize write FSize;
      {This represents the name of the source file. When an archive is opened,
      this will be sent to ''.}
      property SourceFile: String read FSourceFile write FSourceFile;
  end;

  {This object represents a file archive composed of TArchiveEntry objects.
  When the archive is built, all files will be copied back-to-back into a
  single file. A data packet will be written to the end of the file that
  contains the individual file information.<P>
  <P>
  The TFileArchive object can be used to create self-extracting archives
  (compression is not handled by this object). To do this, create a
  TFileArchive object and add files to it. The first file added MUST be
  the self-extracting stub file; the files that follow it can be anything
  you want.  The self-extracting stub file must create a TFileArchive
  that points to itself. It can then extract the files that follow it in
  the archive.<P>
  <P>
  The ArchiveName property is set in the CreateEx constructor. The Entry
  and EntryCount properties provide access to the individual TArchiveEntry
  objects. The Signature property can be used to sign the archive and prevent
  truncation of the archive. The Size property indicates the current size
  of the archive. The AddFile method allows you to add files; the
  ClearAllEntries method will remove all items. The BuildArchive method
  is used to build the entire archive (the OnBuild, OnBuildProgress and
  OnBuildTotal events will be triggered).  The OpenArchive method
  must be called before the Extract method can be called to extract file.}
  TFileArchive = class(TComponent)
  private
    FArchiveName      : String;
    FFiles            : TStringList;
    FOnBuild          : TArchiveFileEvent;
    FOnBuildProgress  : TSizedNotify;
    FOnBuildTotal     : TArchiveFileEvent;
    FOnExtract        : TArchiveFileEvent;
    FOnExtractProgress: TSizedNotify;
    FOnExtractTotal   : TArchiveFileEvent;
    FSignature        : String;
    FSize             : LongInt;
  protected
    function  GetEntry(Index: Integer): TArchiveEntry; virtual;
    function  GetEntryCount: Integer; virtual;
    function  ObtainFileSizes(Extract: Byte): LongInt; virtual;
    function  ReadString(F: THandleStream): String; virtual;
    procedure SetSignature(Value: String); virtual;
    procedure UpdateFile(E: TArchiveEntry); virtual;
    procedure WriteString(F: THandleStream; S: String); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateEx(AOwner: TComponent; ArchiveName: String); virtual;
    destructor Destroy; override;
    function  AddFile(FileName: String): TArchiveEntry; virtual;
    procedure BuildArchive; virtual;
    procedure ClearAllEntries; virtual;
    procedure Extract(Dir: String); virtual;
    function  FindFile(FileName: String): Integer; virtual;
    function  IndexOfEntry(Entry: TArchiveEntry): Integer; virtual;
    function  IsDir(Dir: String): Boolean; virtual;
    procedure OpenArchive; virtual;
    procedure RefreshFiles; virtual;
    {Returns the Entry with the specified Index.}
    property Entry[Index: Integer]: TArchiveEntry read GetEntry;
    {Returns the number of entries in the archive.}
    property EntryCount: Integer read GetEntryCount;
    {Size of the archive.}
    property Size: LongInt read FSize;
  published
    {Name of the archive file. This should include the entire path as well.}
    property ArchiveName: String read FArchiveName write FArchiveName;
    {Event triggered whenever bytes are written.}
    property OnBuild: TArchiveFileEvent read FOnBuild write FOnBuild;
    {Event triggered whenever bytes are written for a specific file.}
    property OnBuildProgress: TSizedNotify read FOnBuildProgress write FOnBuildProgress;
    {Event triggered as the overall build progresses.}
    property OnBuildTotal: TArchiveFileEvent read FOnBuildTotal write FOnBuildTotal;
    {Event triggered whenever bytes are written.}
    property OnExtract: TArchiveFileEvent read FOnExtract write FOnExtract;
    {Event triggered whenever bytes are written for a specific file.}
    property OnExtractProgress: TSizedNotify read FOnExtractProgress write FOnExtractProgress;
    {Event triggered as the overall extraction progresses.}
    property OnExtractTotal: TArchiveFileEvent read FOnExtractTotal write FOnExtractTotal;
    {Signature that will br written to the archive. To open an archive, the
    signature must match that of the archive being opened}
    property Signature: String read FSignature write SetSignature;
  end;

  {Event type for API progress bar changes.}
  TAPIProgress = procedure(Sender: TComponent; var Progress: LongInt) of object;

  {This object is a simple progress bar. Since it does not use the Forms or
  Dialogs unit, it is rather small and can be used in console-only
  applications. The BarMax and BarUsed properties control how much of the
  progress bar is displayed. The Execute and Halt methods are used to
  show and close the progress bar.<P>
  <P>
  The progress bar in TAPIProgressBar can also be controlled using windows
  messages. If a WM_BARSIZE message is sent to the window, then the LParam
  parameter of the message will be applied to the BarMax property. A
  WM_BARUP will cause call the BarUp method with the value of the LParam
  parameter of the message. These messages should be sent the window
  specified by the WinHandle property.}
  TAPIProgressBar = class(TComponent)
  private
    { Private declarations }
    FBarMax    : LongInt;
    FBarUsed   : LongInt;
    FBarX      : Integer;
    FBarX1     : Integer;
    FBarY      : Integer;
    FBarY1     : Integer;
    FFontSize  : Integer;
    FIcon      : TIcon;
    FIconHandle: Integer;
    FIconName  : String;
    FOnBarUp   : TAPIProgress;
    FTextString: String;
    FWinHandle : THandle;
    FWinRec    : TRect;
  protected
    { Protected declarations }
    procedure SetBarMax(Value: LongInt); virtual;
    procedure SetBarUsed(Value: LongInt); virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BarUp(Amount: LongInt); virtual;
    procedure DrawBar; virtual;
    procedure DrawWindow; virtual;
    procedure Execute(ParentHandle: THandle); virtual;
    procedure Halt; virtual;
    procedure Paint; virtual;
    procedure SizeAll; virtual;
    {The handle of the progress window.}
    property WinHandle: THandle read FWinHandle;
  published
    { Published declarations }
    {The maximum value for the progress bar. If the BarUsed property exceeds
    the BarMax property, then BarUsed will be assumed to be equal to BarMax.}
    property BarMax: LongInt read FBarMax write SetBarMax;
    {The portion of the progress bar that is currently being used.}
    property BarUsed: LongInt read FBarUsed write SetBarUsed;
    {The icon that will be displayed in the progress bar. If this property
    is set, then the IconName property will be ignored.}
    property Icon: TIcon read FIcon;
    {The name of an icon resource that is attached to the executable file or
    DLL. If this property is blank, or the resource does not exist, then
    no icon will be displayed. This property will be ignored if the
    Icon property is set.}
    property IconName: String read FIconName write FIconName;
    {The event that will be triggered every time the progress bar state is
    changed.}
    property OnBarUp: TAPIProgress read FOnBarUp write FOnBarUp;
    {Text string that will be displayed in the progress bar.}
    property TextString: String read FTextString write FTextString;
  end;

  {Different types of command line options.<P>
  clAny: Implies all the other types of command line types.<BR>
  clSimple: The option is simply the option name.  The option does not have
  a value, but is simply enabled or disabled. For example, option NAR
  would be enabled on this command line: PROGRAM.EXE NAR<BR>
  clEqual: The option name is followed by an equal sign and the value. Presence
  on the command line indicates the option is enabled. For example, the option
  NAR is set to 2 and enabled in the following example: PROGRAM.EXE NAR=2<BR>
  clSlash: The option name is preceeded by a slash and optionally followed by
  a value. If present on the command line, then the option is enabled.
  For example, NAR has a value of J and is enabled in the following example:
  PROGRAM.EXE \NARJ<BR>
  clDash: The option name is preceeded by a dash and optionally followed by
  a value. If present on the command line, then the option is enabled.
  For example, NAR has a value of K and is enabled in the following example:
  PROGRAM.EXE -NARK<BR>}
  TCommandLineTypes = (clAny, clSimple, clEqual, clSlash, clDash);
  {Set of TCommandLineTypes.}
  TCommandLineTypeSet = set of TCommandLineTypes;

  {This object encapsulates the command line. The EXE property is just the file
  name of the executable while the FullEXE is the full path and file name of
  the executable - this is the same as the ParamStr(0) function. The
  Count property indicates how many options were found on the command line,
  the same value as ParamCount. An option can be either xxx, xxx=yyy,
  /xxxyyy, or -xxxyy.  The CommandSet method will check to see if xxx was on
  the command line. The CommandValue method will return the yyy portion of a
  specified xxx option, while CommandIntValue will attempt to turn yyy into
  and integer. If CaseSensitive is enabled, then the command line options have
  to match the case you specify before they are considered valid.}
  TCommandLine = class(TComponent)
    private
      FCaseSensitive: Boolean;
      FParams       : TStringList;
    protected
      function  GetCount: Integer; virtual;
      function  GetEXE: String; virtual;
      function  GetFullEXE: String; virtual;
      function  GetIndex(Param: String; OptionTypes: TCommandLineTypeSet;
                         StartIndex: Integer; var Value: String): Integer; virtual;
      procedure SetCaseSensitive(Value: Boolean); virtual;
    public
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      function  CommandIntValue(Param: String; OptionTypes: TCommandLineTypeSet): Integer; virtual;
      function  CommandLine: String; virtual;
      procedure CommandList(Param: String; OptionTypes: TCommandLineTypeSet; L: TStringList); virtual;
      function  CommandSet(Param: String; OptionTypes: TCommandLineTypeSet): Boolean; virtual;
      function  CommandValue(Param: String; OptionTypes: TCommandLineTypeSet): String; virtual;
      procedure ScanCommandLine; virtual;
      {Number of options on the command line.}
      property Count: Integer read GetCount;
      {Simple file name of the executable.}
      property EXE: String read GetEXE;
      {Full path and file name of the executable.}
      property FullEXE: String read GetFullEXE;
    published
      {Flag indicating that all parameters (but not their values - if any)
      should be looked up in a case-sensitive fashion. By default, this is
      disabled.}
      property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
  end;

  {A simple registration code component. Every application should use
  unique ProgramStr1, ProgramStr2, PadStr and Key values. To obtains a code,
  use the BuildCode method. To see if a particular user name and code
  are valid, use the CheckCode method.}
  TRegCode = class(TComponent)
  private
    FKey          : array[0..3] of Byte;
    FMaxNameLength: Integer;
    FPadStr       : String;
    FProgramStr1  : String;
    FProgramStr2  : String;
  protected
    function  KeyString(S: String): LongInt; virtual;
    function  PadString(S: String): String; virtual;
    procedure SetKey(Index: Integer; Value: Byte); virtual;
    procedure SetMaxNameLength(Value: Integer); virtual;
    procedure SetPadStr(Value: String); virtual;
    function  Yank(Key, Base: Byte; S: String): Byte; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function BuildCode(UserName: String): String;
    function CheckCode(UserName, Code: String): Boolean; virtual;
    {Sets the individual key values.}
    property Key[Index: Integer]: Byte write SetKey;
  published
    {Maximum length of names allowed.}
    property MaxNameLength: Integer read FMaxNameLength write SetMaxNameLength;
    {The padding string that will be used to fill in the remaining space
    that the UserName doesn't use.}
    property PadStr: String read FPadStr write SetPadStr;
    {One of two strings that should be assigned unique values.}
    property ProgramStr1: String read FProgramStr1 write FProgramStr1;
    {One of two strings that should be assigned unique values.}
    property ProgramStr2: String read FProgramStr2 write FProgramStr2;
  end;

  {The standard Delphi TINIFile component has a 64K limitation. Further
  investigation showed that this was not due to Delphi, but to the WinAPI. This
  problem is apparent in both 16bit and 32bit Windows platforms. This is the
  replacement to the TINIFile component that solves this problem. This
  component is not as fast as the WinAPI calls - especially when the INI file
  is over 64K - but it will always work. For best results, set Delay = True and
  call WriteFile when you have finished manipulating the file. The methods
  and properties of this object correspond to the TINIFile object.}
  THugeINI = class(TObject)
  private
    FDelay   : Boolean;
    FFileName: String;
    FKeys    : TList;
    FSections: TStringList;
    FValues  : TList;
  protected
    function  AddKey(Section: Integer; Key: String): Integer; virtual;
    function  AddSection(Section: String): Integer; virtual;
    procedure ExtractKeyValue(Line: String; var Key: String; var Value: String); virtual;
    function  ExtractSection(Line: String): String; virtual;
    function  GetGenericByIndex(Section: Integer; IndexType: Integer): TStringList; virtual;
    function  GetGeneric(Section: String; IndexType: Integer): TStringList; virtual;
    function  GetLocation(Section, Key: String; var S, K: Integer; Create: Boolean): Boolean; virtual;
    function  GetSectionCount: Integer; virtual;
    function  GetValue(Section, Key: Integer): String; virtual;
    function  KeyIndex(Section: Integer; Key: String): Integer; virtual;
    procedure ReadFile; virtual;
    function  SectionIndex(Section: String): Integer; virtual;
    procedure SetValue(Section, Key: Integer; Value: String); virtual;
    {Number of sections in the INI file.}
    property SectionCount: Integer read GetSectionCount;
    {Returns the set of keys associated with the specified section.}
    property Keys[Section: Integer]: TStringList index 0 read GetGenericByIndex;
    {Returns or sets a single string value of the specified section and key.}
    property Value[Section, Key: Integer]: String read GetValue write SetValue;
    {Returns the set of values associated with the specified section.}
    property Values[Section: Integer]: TStringList index 1 read GetGenericByIndex;
  public
    constructor Create(FileName: String); virtual;
    destructor Destroy; override;
    procedure EraseSection(Section: String); virtual;
    function  ReadBool(Section, Key: String; Default: Boolean): Boolean; virtual;
    function  ReadInteger(Section, Key: String; Default: LongInt): LongInt; virtual;
    procedure ReadSection(Section: String; Target: TStrings); virtual;
    procedure ReadSections(Target: TStrings); virtual;
    procedure ReadSectionValues(Section: String; Target: TStrings); virtual;
    function  ReadString(Section, Key: String; Default: String): String; virtual;
    procedure RemoveKey(Section, Key: String); virtual;
    procedure WriteBool(Section, Key: String; Default: Boolean); virtual;
    procedure WriteFile; virtual;
    procedure WriteInteger(Section, Key: String; Default: LongInt); virtual;
    procedure WriteString(Section, Key: String; Default: String); virtual;
    {Delay writing the INI file until it is closed or manually written. This
    can increase performance since the INI file is usually written everytime
    it is modified.}
    property Delay: Boolean read FDelay write FDelay;
  end;

  {This is a TMemoryStream descendant that is easy to use as a form of
  communication involving a single stream object. Before starting to read
  values from the stream, remember to Seek to the beginning.}
  TMessageStream = class(TMemoryStream)
    private
    protected
    public
      function  ReadOutBoolean: Boolean; virtual;
      function  ReadOutDateTime: TDateTime; virtual;
      function  ReadOutInteger: LongInt; virtual;
      function  ReadOutString: String; virtual;
      procedure WriteInBoolean(B: Boolean); virtual;
      procedure WriteInDateTime(D: TDateTime); virtual;
      procedure WriteInInteger(I: LongInt); virtual;
      procedure WriteInString(S: String); virtual;
  end;

  {This component provides a window that can receive window messages. This
  is particularly useful in console applications. The ID parameter is used
  to construct the windows caption, via the BuildCaption method. The handle
  of the window is in the WinHandle property. The FindUniqueID method can
  be used to ensure that the ID (and window title) are unique. When a
  message is received by the window, the OnMessage event is triggered, or
  the HandleMessage method can be overridden.}
  TMiniWin = class(TComponent)
    private
      FID       : Word;
      FOnMessage: TMiniEvent;
      FWinHandle: LongInt;
    protected
      function  BuildCaption(ID: Word): TDataCaption; virtual;
      function  HandleMessage(Msg, WParam, LParam: LongInt): LongInt; virtual;
      procedure SetID(Value: Word); virtual;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function  FindUniqueID: Word; virtual;
      {Handle of the mini window.}
      property WinHandle: LongInt read FWinHandle;
    published
      {ID of the object.}
      property ID: Word read FID write SetID;
      {Event that is triggered when a message is received.}
      property OnMessage: TMiniEvent read FOnMessage write FOnMessage;
  end;

  {This component allows interprocess (not interPC) communication between
  applications or within applications. This component can be used to
  communicate between 16bit and 32bit applications. This component uses
  a form of communication very similar to DDE, but is faster and allows for
  transmission and receiving of blocks of data.<P>
  <P>
  Before communicating with the remote TCopyData, set the TargetID to the
  ID of the remote TCopyData. Use the LocateTarget method to establish the
  connection. The state of the connection can be checked at any time via the
  StillLinked method. Data can be sent via the SendData method or requested
  via the RequestData method. To make data transmission easier, the
  ReadStream and WriteStream methods will automatically read or write a
  TMessageStream from the remote TCopyData. The OnIncoming and OnRequest
  events are triggered when data is received or requested.<P>
  <P>
  <B>NOTE:</B> You cannot send pointers via this way of communication, unless
  both the sending and receiving TCopyData are within the same process.}
  TCopyData = class(TMiniWin)
    private
      FGotStuff  : Boolean;
      FDebugMsg  : Boolean;
      FOnIncoming: TIncomingDataEvent;
      FOnRequest : TRequestDataEvent;
      FTarget    : LongInt;
      FTargetID  : Word;
    protected
      function  BuildCaption(ID: Word): TDataCaption; override;
      function  DataRequested(Data: LongInt): LongInt; virtual;
      function  IncomingData(Data: LongInt; DataPointer: Pointer; SizeOfData: LongInt): LongInt; virtual;
      function  HandleMessage(Msg, WParam, LParam: LongInt): LongInt; override;
      function  ReceiveData(Data: PCopyDataStruct): LongInt; virtual;
    public
      constructor Create(AOwner: TComponent); override;
      function  LocateTarget: Boolean; virtual;
      procedure ReadStream(Stream: TMemoryStream; DataPointer: Pointer; Size: LongInt); virtual;
      procedure RequestData(Request: Word; Wait: Boolean); virtual;
      function  SendData(Data: LongInt; DataPointer: Pointer; SizeOfData: LongInt): LongInt; virtual;
      function  StillLinked: Boolean; virtual;
      procedure WriteStream(Data: LongInt; Stream: TMemoryStream); virtual;
      {Handle of the target window that will recieve the WM_COPYDATA
      messages. Usually, this is the handle the window created by another
      TCopyData component.}
      property Target: LongInt read FTarget write FTarget;
    published
      {Event that is triggered when data is received by the component.}
      property OnIncoming: TIncomingDataEvent read FOnIncoming write FOnIncoming;
      {Event that is triggered when data is requested from the TCopyData object.}
      property OnRequest: TRequestDataEvent read FOnRequest write FOnRequest;
      {ID of the target TCopyData object.}
      property TargetID: Word read FTargetID write FTargetID;
  end;

  {This component allows for template-type creation of text documents. A
  template is either loaded from a file, via LoadTemplate, or built up in the
  Template property. An identical copy will appear in the Document property.
  Use the ReplaceText method to replace text strings within the Document
  property. The template can also be specified through the TemplateFile
  property.}
  TDocumentTemplate = class(TComponent)
    private
      FDocument    : TStringList;
      FTemplate    : TStringList;
      FTemplateFile: String;
    protected
      procedure SetTemplate(Value: TStringList); virtual;
      procedure SetTemplateFile(Value: String); virtual;
    public
      constructor Create(AOwner: TComponent); override;
      constructor CreateEx(AOwner: TComponent; TemplateName: String); virtual;
      destructor Destroy; override;
      procedure LoadTemplate(TemplateName: String); virtual;
      function  ReplaceText(Search, Replace: String; CaseSensitive: Boolean): LongInt; virtual;
      {This property contains the document that is being built from the
      Template property. When the template is loaded via the Create
      constructor or the Template property is modified, a copy is made to the
      Document property. Any ReplaceText method calls will modify this
      property.}
      property Document: TStringList read FDocument;
    published
      {The TStringList that contains the template. It can be loaded in the
      CreateEx constructor, or it can be built up on the fly. This copy
      will not be modifed by ReplaceText command.}
      property Template: TStringList read FTemplate write SetTemplate;
      {The name of a file that should be loaded as the template.}
      property TemplateFile: String read FTemplateFile write SetTemplateFile;
  end;

const
  {Block size of archive reading and writing.}
  ArchiveBlockSize = 2048;
  {Message used to cause a TAPIProgressBar to move the bar up. The LParam
  part of the message is the amount to increase the bar by.}
  WM_BARUP         = WM_USER + 001;
  {Message indicating that the TAPIProgressBar should be resized.}
  WM_BARSIZE       = WM_USER + 003;

var
  {This will always contain the system wide temporary path.}
  TemporaryPath    : String;
  {This will contain the full path to the Window directory.}
  WindowsPath      : String;
  {This will contain the full path to the Windows System directory.}
  WindowsSystemPath: String;

function  AddBackSlash(const S: String): String;
function  BoolToStr(B: Boolean): String;
function  CompareVersions(A, B: String): Integer;
procedure CopyFile(Source, Dest: String; Event: TSizedNotify; Splice: Boolean);
procedure CopySegment(Source, Dest: String; Start, Stop, BlockSize: LongInt;
                      Splice: Boolean; Event: TSizedNotify);
procedure Debug(Level: Byte; Module, Area: LongInt; Msg: String);
procedure DebugEx(Level: Byte; Module, Area, Template: String; const Args: array of const);
function  DebugRelink: Boolean;
function  DeleteDirectory(Dir: String; IncludeSubDirs: Boolean;
                          FileNotify, DirNotify: TDeletionNotify;
                          var RebootRequired: Boolean): Boolean;
function  DeleteJustDirectory(Dir: String): Boolean;
function  DirExists(Dir: String): Boolean;
function  DirString(Dir: String): String;
function  ExtractRoot(AbsoluteDir: String): String;
procedure ForceCreateDirectories(Dir: string);
procedure ForceRemoveDirectories(Dir: String);
function  GetEnvVar(EnvVar: String): String;
procedure GetFiles(Dir, FileMask: String; FileList: TStrings);
function  GetFileSize(FileName: String): LongInt;
function  GetFileVersion(FileName: String): String;
procedure GetSubdirectories(Dir: String; var SubDirs: TStringList);
function  GetUniqueFileName(Dir, FileTemplate, ExtensionTemplate: String): String;
function  IsSet(A, Attributes: LongInt): Boolean;
function  IsAbsolutePath(Dir: String): Boolean;
function  IsRelativePath(Dir: String): Boolean;
function  MakeAbsolute(BaseDir, RelativeTarget: String): String;
function  MakeRelative(BaseDir, FullTarget: String): String;
function  Max(A, B: LongInt): LongInt;
function  Min(A, B: LongInt): LongInt;
function  Parse(var S: String; Separator: String): String;
function  PMsgDlg(Msg, Caption: String; TextType: Word): Integer;
procedure SetAttribute(A: LongInt; var Attributes: LongInt; Value: Boolean);
function  StripBackSlash(const S: String): String;
function  StripCRLF(P: PChar): PChar;
function  Wrap(S: String): String;
function  YieldProcess: Boolean;

implementation

var
  APIWinClass : TWndClass;
  DebugID     : String;
  DebugPort   : TCopyData;
  MiniWinClass: TWndClass;
  MiniWindows : TList;
  UsedWindows : TList;

{Create and initialize the object.}
constructor TFileArchive.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FArchiveName:= '';
  FFiles:= TStringList.Create;
  FSignature:= 'Aloha Oi File Archive';
  FSize:= SizeOf(LongInt) + Length(FSignature) + 1;
end;

{Create the object and initialize with the name of an archive.}
constructor TFileArchive.CreateEx(AOwner: TComponent; ArchiveName: String);
begin
  Create(AOwner);
  FArchiveName:= ArchiveName;
end;

{Deallocate and destroy the object.}
destructor TFileArchive.Destroy;
begin
  ClearAllEntries;
  FFiles.Free;
  inherited Destroy;
end;

{This will add an entry based on the file specified in the FileName parameter.}
function TFileArchive.AddFile(FileName: String): TArchiveEntry;
begin
  Result:= NIL;
  {Make sure that the file exists.}
  if not(FileExists(FileName)) then EXIT;
  {Create the item and the file's name and item to the list.}
  Result:= TArchiveEntry.Create;
  FFiles.AddObject(FileName, Result);
  {Get the file name and intialize all names/dirs.}
  Result.SourceFile:= FileName;
  Result.ArchiveName:= ExtractFileName(FileName);
  Result.ExtractName:= ExtractFileName(FileName);
  UpdateFile(Result);
end;

{Build the archive.}
procedure TFileArchive.BuildArchive;
var
  AHandle     : Integer;
  ArchiveTotal: LongInt;
  AStream     : THandleStream;
  Block       : array[0..ArchiveBlockSize] of Byte;
  Done        : LongInt;
  E           : TArchiveEntry;
  FileTotal   : LongInt;
  FStream     : THandleStream;
  FHandle     : Integer;
  i           : Integer;
  Total       : LongInt;
begin
  RefreshFiles;
  {Create the file and check to see if the handle is legit.}
  AHandle:= FileCreate(FArchiveName);
  if AHandle > -1 then
    try
      {Create the stream.}
      AStream:= THandleStream.Create(AHandle);
      try
        {Get the full size of the archive.}
        Total:= ObtainFileSizes(0);
        ArchiveTotal:= 0;
        {Go through each file.}
        for i:= 0 to FFiles.Count - 1 do
          begin
            FileTotal:= 0;
            {Create a handle to the source file and see if it is legit.}
            FHandle:= FileOpen(FFiles[i], fmOpenRead or fmShareDenyNone);
            if FHandle > -1 then
              try
                {Create the file stream to the source file.}
                FStream:= THandleStream.Create(FHandle);
                try
                  repeat
                    {Copy the file.}
                    Done:= FStream.Read(Block, ArchiveBlockSize);
                    AStream.Write(Block, Done);
                    inc(ArchiveTotal, Done);
                    inc(FileTotal, Done);
                    YieldProcess;
                    {Fire off the events.}
                    if Assigned(FOnBuildProgress) then
                      FOnBuildProgress(Self, Done);
                    if Assigned(FOnBuild) then
                      FOnBuild(Self, FileTotal, FStream.Size, i);
                    if Assigned(FOnBuildTotal) then
                      FOnBuildTotal(Self, ArchiveTotal, Total, i);
                  until Done < ArchiveBlockSize;
                  DebugEx(2, 'TFileArchive', 'Build', 'File built into archive (%s): (%d)',
                          [FFiles[i], FStream.Size]);
                finally
                  FStream.Free;
                end;
              finally
                FileClose(FHandle);
              end
            else raise EArchiveError.Create('Unable to open file: ' + FFiles[i]);
          end;
        {Obtain the total file size.}
        Total:= AStream.Size;
        {Now write the header.}
        WriteString(AStream, FSignature);
        {Write each file's entry.}
        for i:= 0 to EntryCount - 1 do
          begin
            {Get the item.}
            E:= Entry[i];
            {Write the name.}
            WriteString(AStream, E.ArchiveName);
            {Write the time stamp, size, and index.}
            AStream.Write(E.FDateTime, SizeOf(E.FDateTime));
            AStream.Write(E.FSize, SizeOf(E.FSize));
            AStream.Write(E.FOffset, SizeOf(E.FOffset));
          end;
        {Write the index to the header.}
        AStream.Write(Total, SizeOf(Total));
      finally
        AStream.Free;
      end;
    finally
      FileClose(AHandle);
    end
  else raise EArchiveError.Create('Unable to build to archive: ' + FArchiveName);
end;

{Clear all the entries from the archive.}
procedure TFileArchive.ClearAllEntries;
var
  E: TArchiveEntry;
begin
  {Dispose of each entry.}
  while FFiles.Count > 0 do
    begin
      E:= Entry[0];
      E.Free;
      FFiles.Delete(0);
    end;
  FSize:= SizeOf(LongInt) + Length(FSignature) + 1;
end;

{Extract all the files to the specified directory.}
procedure TFileArchive.Extract(Dir: String);
var
  AHandle     : Integer;
  ArchiveTotal: LongInt;
  AStream     : THandleStream;
  Block       : array[0..ArchiveBlockSize] of Word;
  Done        : LongInt;
  E           : TArchiveEntry;
  FHandle     : Integer;
  FileTotal   : LongInt;
  FStream     : THandleStream;
  i           : Integer;
  S           : String;
  Size        : LongInt;
  Total       : LongInt;
begin
  {Make sure the directory exists.}
  if not(IsDir(Dir)) then
    raise EArchiveError.Create('Destination directory does not exist.');
  {Make sure the directory has a trailing slash.}
  Dir:= AddBackSlash(Dir);
  {Obtain a handle to the archive and make sure it is legit.}
  AHandle:= FileOpen(FArchiveName, fmOpenRead or fmShareDenyNone);
  if AHandle > -1 then
    try
      {Create a stream from the archive.}
      AStream:= THandleStream.Create(AHandle);
      try
        Total:= ObtainFileSizes(1);
        ArchiveTotal:= 0;
        {Go through each entry.}
        for i:= 0 to EntryCount - 1 do
          begin
            {Get the item and make sure we can extract it.}
            E:= Entry[i];
            if E.Extract then
              begin
                {If the items extraction dir is non empty, then
                ensure that it ends in a trailing slash.}
                if (Length(E.ExtractDir) > 0) then E.ExtractDir:= AddBackSlash(E.ExtractDir);
                {Obtain the items extraction directory.}
                S:= E.ExtractDir;
                if S = '' then S:= Dir;
                {Create a handle to the new file and make sure it is legit.}
                FHandle:= FileCreate(S + E.ExtractName);
                if FHandle > -1 then
                  try
                    {Create a stream to the new file.}
                    FStream:= THandleStream.Create(FHandle);
                    try
                      {Jump to the correct part of the archive file.}
                      AStream.Seek(E.FOffset, 0);
                      {Initalize for the copy.}
                      Size:= E.Size;
                      FileTotal:= 0;
                      repeat
                        {See what size we should copy.}
                        if Size > ArchiveBlockSize then Done:= ArchiveBlockSize
                        else Done:= Size;
                        {Read from archive, write to new file.}
                        AStream.Read(Block, Done);
                        FStream.Write(Block, Done);
                        {Adjust the number we still need to copy.}
                        dec(Size, Done);
                        inc(ArchiveTotal, Done);
                        inc(FileTotal, Done);
                        YieldProcess;
                        {Fire off the event.}
                        if Assigned(FOnExtractProgress) then
                          FOnExtractProgress(Self, Done);
                        if Assigned(FOnExtract) then
                          FOnExtract(Self, FileTotal, E.Size, i);
                        if Assigned(FOnExtractTotal) then
                          FOnExtractTotal(Self, ArchiveTotal, Total, i);
                      until Size = 0;
                      {Adjust the time and date.}
                      FileSetDate(FStream.Handle, DateTimeToFileDate(E.DateTime));
                      DebugEx(2, 'TFileArchive', 'Extract', 'File extracted from archive (%s): (%d)',
                              [FFiles[i], FStream.Size]);
                    finally
                      FStream.Free;
                    end;
                  finally
                    FileClose(FHandle);
                  end
                else
                  raise EArchiveError.Create('Unable to create file:' +
                                                  S + E.ExtractName);
              end;
          end;
      finally
        AStream.Free;
      end;
    finally
      FileClose(AHandle);
    end
  else raise EArchiveError.Create('Unable to open the archive.');
end;

{This will return the index of an entry whose
<T TArchiveEntry.ArchiveName>ArchiveName</T> matches the one specified.}
function TFileArchive.FindFile(FileName: String): Integer;
var
  i: Integer;
  S: String;
begin
  Result:= -1;
  S:= AnsiUpperCase(FileName);
  for i:= 0 to EntryCount - 1 do
    if AnsiUpperCase(Entry[i].ArchiveName) = S then Result:= i;
end;

{Read method for the Entry property.}
function TFileArchive.GetEntry(Index: Integer): TArchiveEntry;
begin
  Result:= TArchiveEntry(FFiles.Objects[Index]);
end;

{Read method for the EntryCount property.}
function TFileArchive.GetEntryCount: Integer;
begin
  Result:= FFiles.Count;
end;

{Returns the index of the specified entry. If a -1 is returned, then the
TArchiveEntry was not found.}
function TFileArchive.IndexOfEntry(Entry: TArchiveEntry): Integer;
begin
  Result:= FFiles.IndexOfObject(Entry);
end;

{See if this is a directory.}
function TFileArchive.IsDir(Dir: String): Boolean;
var
  Attr: Integer;
begin
  while Dir[Length(Dir)] = '\' do Delete(Dir, Length(Dir), 1);
  Dir:= Dir + '\.';
  Attr:= FileGetAttr(Dir);
  Result:= (Attr and faDirectory) = faDirectory;
end;

{Returns the total size of all files based on the Extract parameter:
0: All files
1: Extract Only
2: Non-Extract Only.}
function TFileArchive.ObtainFileSizes(Extract: Byte): LongInt;
var
  E: TArchiveEntry;
  i: Integer;
begin
  Result:= 0;
  for i:= 0 to EntryCount - 1 do
    begin
      E:= Entry[i];
      case Extract of
        0: inc(Result, E.Size);
        1: if E.Extract then inc(Result, E.Size);
        2: if not(E.Extract) then inc(Result, E.Size);
      end;
    end;
end;

{This will open the archive and read the entries.
If no archive exists, then the entries will be simply cleared.}
procedure TFileArchive.OpenArchive;
var
  DateTime  : TDateTime;
  E         : TArchiveEntry;
  FileHandle: Integer;
  FileStream: THandleStream;
  Index     : LongInt;
  Name      : String;
  S         : String;
  Size      : LongInt;
  Total     : LongInt;
begin
  ClearAllEntries;
  if FileExists(FArchiveName) then
    begin
      {Get a file handle.}
      FileHandle:= FileOpen(FArchiveName, fmOpenRead or fmShareDenyNone);
      {See if we generate an error.}
      if FileHandle > -1 then
        try
          {Create a file stream.}
          FileStream:= THandleStream.Create(FileHandle);
          try
            if (FileStream.Size < SizeOf(Total)) then
              raise EArchiveError.Create('File is too small.  The file is not an archive.');
            {This will put us in a position to read the Data Offset.}
            FileStream.Seek(-SizeOf(Total), 2);
            {Read the Data Offset and make sure it is legit.}
            FileStream.Read(Total, SizeOf(Total));
            if Total > FileStream.Size then
              raise EArchiveError.Create('Data Offset is illegal.  This file is not an archive.');
            {Jump to the spot specified by the Data Offset.}
            FileStream.Seek(Total, 0);
            {Read the signature and make sure it is correct.}
            S:= ReadString(FileStream);
            if S <> FSignature then
              raise EArchiveError.Create('Invalide archive signature.');
            {Begin processing each entry.}
            try
              while FileStream.Position < FileStream.Size - SizeOf(Total) do
                begin
                  {Read the name, timestamp, size, and file index.}
                  Name:= ReadString(FileStream);
                  FileStream.Read(DateTime, SizeOf(DateTime));
                  FileStream.Read(Size, SizeOf(Size));
                  FileStream.Read(Index, SizeOf(Index));
                  {Allocate, add, and setup the entry.}
                  E:= TArchiveEntry.Create;
                  E.ArchiveName:= Name;
                  E.SourceFile:= Name;
                  E.DateTime:= DateTime;
                  E.Size:= Size;
                  E.Offset:= Index;
                  E.Extract:= True;
                  E.ExtractDir:= '';
                  E.ExtractName:= Name;
                  FFiles.AddObject(E.ArchiveName, E);
                end;
            except
              on EInOutError do
                raise EArchiveError.Create('Incomplete entry found.  Invalid archive.');
            end;
          finally
            {Free the dynamic stuff.}
            FileStream.Free;
          end;
        finally
          FileClose(FileHandle);
        end
      else raise EArchiveError.Create('File error opening archive.');
    end;
end;

{This will read a string from the stream.}
function TFileArchive.ReadString(F: THandleStream): String;
var
  C: Char;
  i: Integer;
begin
  Result:= '';
  repeat
    i:= F.Read(C, 1);
    if C <> #0 then Result:= Result + C;
    if i = 0 then
      begin
        Result:= '';
        BREAK;
      end;
  until C = #0;
end;

{This will refresh all the file size within the archive.}
procedure TFileArchive.RefreshFiles;
var
  i: Integer;
begin
  FSize:= SizeOf(LongInt) + Length(FSignature) + 1;
  for i:= 0 to EntryCount - 1 do UpdateFile(Entry[i]);
end;

{Write method for the Signature property.}
procedure TFileArchive.SetSignature(Value: String);
begin
  FSize:= FSize - Length(FSignature) - 1;
  FSignature:= Value;
  FSize:= FSize + Length(FSignature) + 1;
end;

{Update the file size and the total archive size.}
procedure TFileArchive.UpdateFile(E: TArchiveEntry);
var
  i: Integer;
begin
  E.Size:= GetFileSize(E.SourceFile);
  {Get the file timestamp.}
  E.DateTime:= FileDateToDateTime(FileAge(E.SourceFile));
  {Increase the size of the archive as necessary.}
  inc(FSize, E.Size);
  inc(FSize, Length(E.ArchiveName) + 1);
  inc(FSize, SizeOf(E.FDateTime));
  inc(FSize, SizeOf(E.FOffset));
  {Set the file offset.}
  i:= FFiles.IndexOfObject(E);
  if i > 0 then E.Offset:= Entry[i - 1].Offset + Entry[i - 1].Size
  else E.Offset:= 0;
end;

{This will write a string.}
procedure TFileArchive.WriteString(F: THandleStream; S: String);
var
  i: Integer;
begin
  for i:= 1 to Length(S) do F.Write(S[i], 1);
  i:= 0;
  F.Write(i, 1);
end;

{Create and initialize the object.}
constructor TArchiveEntry.Create;
begin
  FArchiveName:= '';
  FDateTime:= Now;
  FExtract:= False;
  FExtractDir:= AddBackSlash('');
  FExtractName:= '';
  FSize:= 0;
  FSourceFile:= '';
end;

{Write method for the ExtractDir property.}
procedure TArchiveEntry.SetExtractDir(Value: String);
begin
  FExtractDir:= AddBackSlash(Value);
end;

{Create the object.}
constructor TCommandLine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {Create dynamic stuff.}
  FCaseSensitive:= False;
  FParams:= TStringList.Create;
  ScanCommandLine;
end;

{Destroy the object.}
destructor TCommandLine.Destroy;
begin
  inherited Destroy;
  {Free dynamic stuff.}
  FParams.Free;
end;

{Returns the integer value of the specified option. If the option is either
blank or non-numeric, then -1 is returned.}
function TCommandLine.CommandIntValue(Param: String; OptionTypes: TCommandLineTypeSet): Integer;
var
  i: Integer;
  S: String;
begin
  i:= GetIndex(Param, OptionTypes, 0, S);
  if i <> -1 then
    try
      Result:= StrToInt(S);
    except
      on EConvertError do Result:= -1;
    end
  else Result:= -1
end;

{Returns a list of values that correspond to a particular command line option.}
procedure TCommandLine.CommandList(Param: String; OptionTypes: TCommandLineTypeSet; L: TStringList);
var
  i: Integer;
  S: String;
begin
  L.Clear;
  i:= GetIndex(Param, OptionTypes, 0, S);
  while i <> -1 do
    begin
      L.Add(S);
      i:= GetIndex(Param, OptionTypes, i + 1, S);
    end;
end;

{Indicates if the specified command line option was used.}
function TCommandLine.CommandSet(Param: String; OptionTypes: TCommandLineTypeSet): Boolean;
var
  S: String;
begin
  Result:= GetIndex(Param, OptionTypes, 0, S) <> -1;
end;

{Returns the value of the specified command line option.}
function TCommandLine.CommandValue(Param: String; OptionTypes: TCommandLineTypeSet): String;
begin
  GetIndex(Param, OptionTypes, 0, Result);
end;

{Retreives the command line parameters.}
function TCommandLine.CommandLine: String;
var
  i: Integer;
begin
  Result:= '';
  for i:= 1 to ParamCount do Result:= Result + ParamStr(i) + ' ';
end;

{Read method for the Count property.}
function TCommandLine.GetCount: Integer;
begin
  Result:= ParamCount;
end;

{Read method for the EXE property.}
function TCommandLine.GetEXE: String;
begin
  Result:= ExtractFileName(ParamStr(0));
end;

{Read method for the FullEXE property.}
function TCommandLine.GetFullEXE: String;
begin
  Result:= ParamStr(0);
end;

{Get the index of the specified parameter. The OptionTypes parameter indicates
what sort of command line option types should be searched for. The StartIndex
parameter indicates where in the list the search should start. The Value
parameter returns the value of the option - if any.}
function TCommandLine.GetIndex(Param: String; OptionTypes: TCommandLineTypeSet;
                               StartIndex: Integer; var Value: String): Integer;
var
  i: Integer;
  k: TCommandLineTypes;
  S: String;
begin
  Result:= -1;
  Value:= '';
  {See if all types should be search for.}
  if clAny in OptionTypes then
    for k:= Low(TCommandLineTypes) to High(TCommandLineTypes) do
      OptionTypes:= OptionTypes + [k];
  if OptionTypes = [] then EXIT;
  {Case sensative.}
  if not(FCaseSensitive) then Param:= AnsiUpperCase(Param);
  {Run through all parameters.}
  for i:= StartIndex to FParams.Count - 1 do
    begin
      {Case sensative.}
      if not(FCaseSensitive) then S:= AnsiUpperCase(FParams[i])
      else S:= FParams[i];
      {Search for a XXX}
      if (clSimple in OptionTypes) and (Param = S) then
        begin
          Result:= i;
          Value:= '';
          BREAK;
        end;
      {Search for a XXX=YYY}
      if (clEqual in OptionTypes) and (Pos(Param, S) = 1) and
         (Pos('=', S) > 0) then
        begin
          Result:= i;
          Value:= Copy(S, Pos('=', S) + 1, Length(S));
          BREAK;
        end;
      {Search for a /XXXYYY}
      if (clSlash in OptionTypes) and (Pos('/' + Param, S) = 1) then
        begin
          Result:= i;
          Value:= Copy(S, Length(Param) + 2, Length(S));
          BREAK;
        end;
      {Searc for a -XXXYYY}
      if (clDash in OptionTypes) and (Pos('-' + Param, S) = 1) then
        begin
          Result:= i;
          Value:= Copy(S, Length(Param) + 2, Length(S));
          BREAK;
        end;
      {If found, break out now.}
      if Result <> -1 then BREAK;
    end;
end;

{This will scan the entire command line.}
procedure TCommandLine.ScanCommandLine;
var
  i: Integer;
begin
  FParams.Clear;
  for i:= 1 to ParamCount do FParams.Add(ParamStr(i));
end;

{Write method for the CaseSensitive property. This will cause the command
line to be rescanned if the value changes.}
procedure TCommandLine.SetCaseSensitive(Value: Boolean);
begin
  if FCaseSensitive <> Value then ScanCommandLine;
  FCaseSensitive:= Value;
end;

{This will create and initailize the object.}
constructor TRegCode.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKey[0]:= 0;
  FKey[1]:= 0;
  FKey[2]:= 0;
  FKey[3]:= 0;
  FMaxNameLength:= 20;
  FPadStr:= '12345678901234567890';
  FProgramStr1:= 'Test Application';
  FProgramStr2:= 'version 1.00';
end;

{This will build a code based on a given name.}
function TRegCode.BuildCode(UserName: String): String;
var
  L1, L2, L3: LongInt;
begin
  L1:= KeyString(UserName);
  L2:= KeyString(FProgramStr1);
  L3:= KeyString(FProgramStr2);
  FmtStr(Result, '%x', [L1 xor L2 xor L3]);
end;

{This will see if a given UserName and Code are valid with the current
settings.}
function TRegCode.CheckCode(UserName, Code: String): Boolean;
begin
  Result:= (BuildCode(UserName) = Code);
end;

{This will process an entire string into a long int.}
function TRegCode.KeyString(S: String): LongInt;
var
  b: Byte;
  i: Integer;
  L: LongInt;
begin
  Result:= 0;
  S:= PadString(S);
  for i:= 0 to 3 do
    begin
      b:= Yank(FKey[i], i + 1, S);
      L:= LongInt(b) SHL ((3 - i) * 8);
      Result:= Result or L;
    end;
end;

{This will fill up the string if Len(Pad) > MaxNameLengh.}
function TRegCode.PadString(S: String): String;
begin
  S:= S + FPadStr;
  Result:= Copy(S, 1, FMaxNameLength);
end;

{Write method for the Key property.}
procedure TRegCode.SetKey(Index: Integer; Value: Byte);
begin
  if csDestroying in ComponentState then EXIT;
  if (Index < 0) or (Index > 3) then
    MessageBox(0, 'Invalid key index.', 'RegCode', MB_OK)
  else FKey[Index]:= Value;
end;

{Write method for the MaxNameLength property.}
procedure TRegCode.SetMaxNameLength(Value: Integer);
begin
  if csDestroying in ComponentState then EXIT;
  if (Value < 1) or (Value > 50) then
    MessageBox(0, 'Value must be between 1 and 50 characters.', 'RegCode', 0)
  else FMaxNameLength:= Value;
end;

{Write method for the PadStr property.}
procedure TRegCode.SetPadStr(Value: String);
begin
  if csDestroying in ComponentState then EXIT;
  if (Length(Value) < 1) or (Length(Value) > 50) then
    MessageBox(0, 'Value must be between 1 and 50 characters.', 'RegCode', 0)
  else FPadStr:= Value;
end;

{This will do a yank of characters and XOR them.}
function TRegCode.Yank(Key, Base: Byte; S: String): Byte;
var
  i: Integer;
begin
  i:= Base;
  Result:= Key;
  while i <= Length(S) do
    begin
      Result:= Result XOR Ord(S[i]);
      inc(i, 4);
    end;
end;

{Create the object.}
constructor THugeINI.Create(FileName: String);
begin
  inherited Create;
  {Create dynamic stuff.}
  FKeys:= TList.Create;
  FSections:= TStringList.Create;
  FValues:= TList.Create;
  {Setup the vars.}
  FDelay:= False;
  FFileName:= FileName;
  ReadFile;
end;

{Destroy the object.}
destructor THugeINI.Destroy;
var
  i: Integer;
begin
  WriteFile;
  {Free dynamic stuff.}
  for i:= 0 to FKeys.Count - 1 do TStringList(FKeys[i]).Free;
  FKeys.Free;
  FSections.Free;
  for i:= 0 to FValues.Count - 1 do TStringList(FValues[i]).Free;
  FValues.Free;
  inherited Destroy;
end;

{Add a key and return its index.}
function THugeINI.AddKey(Section: Integer; Key: String): Integer;
begin
  Result:= KeyIndex(Section, Key);
  if Result = -1 then
    begin
      Result:= Keys[Section].Add(Key);
      Values[Section].Add('');
    end;
end;

{Add a section and return its index.}
function THugeINI.AddSection(Section: String): Integer;
var
  L: TStrings;
begin
  Result:= SectionIndex(Section);
  if Result = -1 then
    begin
      Result:= FSections.Add(Section);
      L:= TStringList.Create;
      FKeys.Add(L);
      L:= TStringList.Create;
      FValues.Add(L);
    end;
end;

{Erase an entire section.}
procedure THugeINI.EraseSection(Section: String);
var
  i: Integer;
begin
  i:= SectionIndex(Section);
  if i <> -1 then
    begin
      Keys[i].Free;
      FKeys.Delete(i);
      Values[i].Free;
      FValues.Delete(i);
      FSections.Delete(i);
    end;
end;

{See if the line contains a section value.}
procedure THugeINI.ExtractKeyValue(Line: String; var Key: String; var Value: String);
var
  i: Integer;
begin
  Key:= '';
  Value:= '';
  i:= Pos('=', Line);
  if i > 0 then
    begin
      Key:= Copy(Line, 1, i - 1);
      Value:= Copy(Line, i + 1, Length(Line));
    end;
end;

{See if the line contains a section.}
function THugeINI.ExtractSection(Line: String): String;
var
  i: Integer;
begin
  Result:= '';
  if Length(Line) = 0 then EXIT;
  if Line[1] = '[' then
    begin
      i:= Pos(']', Line);
      if i > 2 then
        begin
          Delete(Line, 1, 1);
          Result:= Copy(Line, 1, i - 2);
          while (Length(Result) > 0) and (Result[1] = ' ') do Delete(Result, 1, 1);
        end;
    end;
end;

{Read method for some TStringList properties.}
function THugeINI.GetGenericByIndex(Section: Integer; IndexType: Integer): TStringList;
begin
  Result:= NIL;
  case IndexType of
    0: Result:= TStringList(FKeys[Section]);
    1: Result:= TStringList(FValues[Section]);
  end;
end;

{Read method for some TStringList properties.}
function THugeINI.GetGeneric(Section: String; IndexType: Integer): TStringList;
var
  i: Integer;
begin
  i:= SectionIndex(Section);
  Result:= GetGenericByIndex(i, IndexType);
end;

{See if the index can be found.}
function THugeINI.GetLocation(Section, Key: String; var S, K: Integer; Create: Boolean): Boolean;
begin
  Result:= False;
  S:= SectionIndex(Section);
  if (S = -1) and Create then S:= AddSection(Section);
  if S <> -1 then
    begin
      K:= KeyIndex(S, Key);
      if (K = -1) and Create then K:= AddKey(S, Key);
      if K <> -1 then Result:= True;
    end;
end;

{Read method for the Value property.}
function THugeINI.GetValue(Section, Key: Integer): String;
var
  L: TStrings;
begin
  L:= Values[Section];
  Result:= L[Key];
end;

{Read method for the SectionCount property.}
function THugeINI.GetSectionCount: Integer;
begin
  Result:= FSections.Count;
end;

{Get the index of the key.}
function THugeINI.KeyIndex(Section: Integer; Key: String): Integer;
var
  i: Integer;
  L: TStrings;
  S: String;
begin
  Result:= -1;
  L:= Keys[Section];
  S:= UpperCase(Key);
  if L <> NIL then
    for i:= 0 to L.Count - 1 do
      if UpperCase(L[i]) = S then
        begin
          Result:= i;
          BREAK;
        end;
end;

{Read a boolean value.}
function THugeINI.ReadBool(Section, Key: String; Default: Boolean): Boolean;
var
  K, S: Integer;
begin
  if GetLocation(Section, Key, S, K, False) then Result:= not(Value[S, K] = '0')
  else Result:= Default;
end;

{Read the INI file.}
procedure THugeINI.ReadFile;
var
  Key: Integer;
  S: String;
  Section: Integer;
  T: TextFile;
  TempKey: String;
  TempSection: String;
  TempValue: String;
begin
  if FileExists(FFileName) then
    begin
      AssignFile(T, FFileName);
      Reset(T);
      Section:= -1;
      while not(EOF(T)) do
        begin
          ReadLn(T, S);
          TempSection:= ExtractSection(S);
          if TempSection <> '' then
            begin
              Section:= AddSection(TempSection);
            end
          else
            begin
              ExtractKeyValue(S, TempKey, TempValue);
              if TempKey <> '' then
                begin
                  Key:= AddKey(Section, TempKey);
                  Value[Section, Key]:= TempValue;
                end;
            end;
        end;
      CloseFile(T);
    end;
end;

{Read a integer value.}
function THugeINI.ReadInteger(Section, Key: String; Default: LongInt): LongInt;
var
  K, S: Integer;
begin
  try
    if GetLocation(Section, Key, S, K, False) then Result:= StrToInt(Value[S, K])
    else Result:= Default;
  except
    on EConvertError do Result:= Default;
  end;
end;

{Read a section.}
procedure THugeINI.ReadSection(Section: String; Target: TStrings);
var
  i: Integer;
begin
  Target.Clear;
  i:= SectionIndex(Section);
  if i <> -1 then Target.Assign(Keys[i]);
end;

{Returns a list of all section names.}
procedure THugeINI.ReadSections(Target: TStrings);
begin
  Target.Clear;
  Target.Assign(FSections);
end;

{Read section values.}
procedure THugeINI.ReadSectionValues(Section: String; Target: TStrings);
var
  i, j: Integer;
  K, V: TStrings;
begin
  Target.Clear;
  i:= SectionIndex(Section);
  if i <> -1 then
    begin
      K:= Keys[i];
      V:= Values[i];
      for j:= 0 to K.Count - 1 do Target.Add(K[j] + '=' + V[j]);
    end;
end;

{Read a string value.}
function THugeINI.ReadString(Section, Key: String; Default: String): String;
var
  K, S: Integer;
begin
  if GetLocation(Section, Key, S, K, False) then Result:= Value[S, K]
  else Result:= Default;
end;

{This will remove a key.}
procedure THugeINI.RemoveKey(Section, Key: String);
var
  K, S: Integer;
begin
  if GetLocation(Section, Key, S, K, False) then
    begin
      Keys[S].Delete(K);
      Values[S].Delete(K);
      if not(FDelay) then WriteFile;
    end;
end;

{Get the index of the section.}
function THugeINI.SectionIndex(Section: String): Integer;
var
  i: Integer;
  S: String;
begin
  Result:= -1;
  S:= UpperCase(Section);
  for i:= 0 to SectionCount - 1 do
    if UpperCase(FSections[i]) = S then
      begin
        Result:= i;
        BREAK;
      end;
end;

{Write method for the Value property.}
procedure THugeINI.SetValue(Section, Key: Integer; Value: String);
var
  L: TStrings;
begin
  L:= Values[Section];
  L[Key]:= Value;
end;

{Write a boolean value.}
procedure THugeINI.WriteBool(Section, Key: String; Default: Boolean);
var
  K, S: Integer;
begin
  if GetLocation(Section, Key, S, K, True) then
    begin
      if Default then Value[S, K]:= '1'
      else Value[S, K]:= '0';
      if not(FDelay) then WriteFile;
    end;
end;

{Write the INI file.}
procedure THugeINI.WriteFile;
var
  i, j: Integer;
  K   : TStrings;
  T   : TextFile;
  V   : TStrings;
begin
  AssignFile(T, FFileName);
  Rewrite(T);
  for i:= 0 to SectionCount - 1 do
    begin
      WriteLn(T, '[' + FSections[i] + ']');
      K:= Keys[i];
      V:= Values[i];
      for j:= 0 to K.Count - 1 do  WriteLn(T, K[j] + '=' + V[j]);
      WriteLn(T);
    end;
  CloseFile(T);
end;

{Write an integer into the INI file.}
procedure THugeINI.WriteInteger(Section, Key: String; Default: LongInt);
var
  K, S: Integer;
begin
  if GetLocation(Section, Key, S, K, True) then
    begin
      Value[S, K]:= IntToStr(Default);
      if not(FDelay) then WriteFile;
    end;
end;

{Write a string into the INI file.}
procedure THugeINI.WriteString(Section, Key: String; Default: String);
var
  K, S: Integer;
begin
  if GetLocation(Section, Key, S, K, True) then
    begin
      Value[S, K]:= Default;
      if not(FDelay) then WriteFile;
    end;
end;

{This will read out a boolean from the stream.}
function TMessageStream.ReadOutBoolean: Boolean;
var
  i: Byte;
begin
  Read(i, SizeOf(i));
  Result:= i = 1;
end;

{This will read out a TTimeStamp from the stream.}
function TMessageStream.ReadOutDateTime: TDateTime;
begin
  Read(Result, SizeOf(TDateTime));
end;

{This will read an integer out of the stream starting at
the current position.}
function TMessageStream.ReadOutInteger: LongInt;
begin
  Read(Result, SizeOf(LongInt));
end;

{This will read a string out of the stream starting at the current
position.}
function TMessageStream.ReadOutString: String;
var
  i: LongInt;
  P: PChar;
begin
  {Get the size of the string.}
  Read(i, SizeOf(LongInt));
  {Get the string.}
  if i = 0 then Result:= ''
  else
    begin
      P:= StrAlloc(i + 1);
      try
        FillChar(P^, i + 1, #0);
        Read(P^, i);
        Result:= StrPas(P);
      finally
        StrDispose(P);
      end;
    end;
end;

{This will write a boolean into the memory stream.}
procedure TMessageStream.WriteInBoolean(B: Boolean);
var
  i: Byte;
begin
  if B then i:= 1
  else i:= 0;
  Write(i, SizeOf(Byte));
end;

{This will write a timestamp into the memory stream.}
procedure TMessageStream.WriteInDateTime(D: TDateTime);
begin
  Write(D, SizeOf(TDateTime));
end;

{This will write an integer into the memory stream.}
procedure TMessageStream.WriteInInteger(I: LongInt);
begin
  Write(I, SizeOf(I));
end;

{This will write a string into the memory stream.}
procedure TMessageStream.WriteInString(S: String);
var
  i: LongInt;
  P: PChar;
begin
  i:= Length(S);
  P:= StrAlloc(i + 1);
  try
    FillChar(P^, i + 1, #0);
    StrPCopy(P, S);
    Write(i, SizeOf(LongInt));
    Write(P^, i);
  finally
    StrDispose(P);
  end;
end;


const
  {Class name assigned to the window.}
  APIClassName = 'DelphiAPIWindow';
  {Message used to indicate that the window needs resizing.}
  WM_SIZEALL   = WM_USER + 002;
  {Size of the X borders.}
  XBorder      = 5;
  {Size of the Y borders.}
  YBorder      = 5;

{Message handling procedure for the TAPIProgress bar window.
Handles the WM_BARUP and WM_BARSIZE messages.}
function APIProgressBarProc(Window: HWnd; Message, WParam: Word;
                            LParam: LongInt): LongInt;
                            {$IFDEF WIN32}
                             stdcall;
                             {$ENDIF}
                             export;
var
  C: TAPIProgressBar;
  i: Integer;
begin
  {Locate the correct object.}
  C:= NIL;
  for i:= 0 to UsedWindows.Count - 1 do
    if TAPIProgressBar(UsedWindows[i]).WinHandle = Window then
      C:= TAPIProgressBar(UsedWindows[i]);
  {Make sure we have an object.}
  if C <> NIL then
    begin
      case Message of
        wm_BarSize: begin
                      C.BarMax:= LParam;
                      C.BarUsed:= 0;
                    end;
        wm_BarUp  : C.BarUp(LParam);
        wm_Destroy: begin
                    end;
        wm_Paint  : C.Paint;
        wm_SizeAll: C.SizeAll;
      end;
    end;
  Result:= DefWindowProc(Window, Message, WParam, LParam);
end;

{Create and initalize the object.}
constructor TAPIProgressBar.Create(AOwner: TComponent);
begin
  {Do the normal stuff.}
  inherited Create(AOwner);
  {Set initial variables.}
  FBarMax:= 100;
  FBarUsed:= 0;
  FFontSize:= 16;
  FIcon:= TIcon.Create;
  FIconHandle:= 0;
  FIconName:= 'TSICON';
end;

{Destroy the object.}
destructor TAPIProgressBar.Destroy;
begin
  Halt;
  inherited Destroy;
end;

{Increment the bar by the specified amount.}
procedure TAPIProgressBar.BarUp(Amount: LongInt);
begin
  if Assigned(FOnBarUp) then FOnBarUp(Self, Amount);
  inc(FBarUsed, Amount);
  if FBarUsed > FBarMax then FBarUsed:= FBarMax;
  if FBarUsed < 0 then FBarUsed:= 0;
  DrawBar;
end;

{Draw the bar.}
procedure TAPIProgressBar.DrawBar;
var
  B     : HBrush;
  DC    : THandle;
  LX, LY: Integer;
begin
  {Get the device context.}
  DC:= GetWindowDC(FWinHandle);
  {Create a brush.}
  B:= CreateSolidBrush(RGB(0, 0, 128));
  {Get the offsets.}
  LY:= GetSystemMetrics(SM_CYDLGFRAME) - 2;
  LX:= GetSystemMetrics(SM_CXDLGFRAME) - 2;
  {Build the rectangle.}
  FWinRec:= Rect(LX + FBarX, LY + FBarY,
                 LX + FBarX + Round(FBarX1 * (FBarUsed / FBarMax)),
                 LY + FBarY + FBarY1);
  {Draw the rectangle.}
  FillRect(DC, FWinRec, B);
  {Delete the brush and release the device context.}
  DeleteObject(B);

  FWinRec:= Rect(LX + FBarX + Round(FBarX1 * (FBarUsed / FBarMax)),
                 LY + FBarY,
                 LX + FBarX + FBarX1,
                 LY + FBarY + FBarY1);
  FillRect(DC, FWinRec, GetStockObject(LTGRAY_BRUSH));
  {Create a brush.}
  ReleaseDC(FWinHandle, DC);
end;

{Draw the window.}
procedure TAPIProgressBar.DrawWindow;
var
  B    : HBrush;
  DC   : THandle;
  F, F1: HFont;
  Pen  : HPen;
  C    : array[0..512] of char;
  LX   : LongInt;
  LY   : LongInt;
  R    : TRect;
  RZ   : TRect;
begin
  {Get the device context.}
  DC:= GetWindowDC(FWinHandle);

  {Get the offsets.}
  LY:= GetSystemMetrics(SM_CYDLGFRAME) - 2;
  LX:= GetSystemMetrics(SM_CXDLGFRAME) - 2;

  {Set the background color and fill it in.}
  SetBkColor(DC, RGB(200, 200, 200));
  GetWindowRect(FWinHandle, RZ);
  R.Top:= LY;
  R.Right:= LX;
  R.Bottom:= (RZ.Bottom - RZ.Top) - (2 * LY);
  R.Left:= (RZ.Right - RZ.Left) - (2 * LX);
  B:= CreateSolidBrush(RGB(200, 200, 200));
  SelectObject(DC, B);
  FillRect(DC, R, B);
  DeleteObject(B);

  {Put the icon in.}
  DrawIcon(DC, XBorder, YBorder, FIconHandle);

  {Set the font.}
  F:= CreateFont(FFontSize, 0, 0, 0, 0,
                 0, 0, 0,
                 1, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
                 DEFAULT_QUALITY, DEFAULT_PITCH,
                 'Arial');
  F1:= SelectObject(DC, F);
  {Draw the text.}
  StrPCopy(C, FTextString);
  LX:= 0;
  if FIconHandle <> 0 then LX:= GetSystemMetrics(SM_CXICON) + 8;
  TextOut(DC, XBorder + LX, YBorder, C, Length(FTextString));
  SelectObject(DC, F1);
  DeleteObject(F);
  {Draw the upper outline of the box.}
  Pen:= CreatePen(PS_SOLID, 2, RGB(100, 100, 100));
  Pen:= SelectObject(DC, Pen);
  {$IFDEF WIN32}
  MoveToEx(DC, FBarX - 3, FBarY + FBarY1 + 3, NIL);
  {$ELSE}
  MoveTo(DC, FBarX - 3, FBarY + FBarY1 + 3);
  {$ENDIF}
  LineTo(DC, FBarX - 3, FBarY - 3);
  LineTo(DC, FBarX + FBarX1 + 3, FBarY - 3);
  Pen:= SelectObject(DC, Pen);
  DeleteObject(Pen);
  {Draw the outer outline of the box.}
  Pen:= CreatePen(PS_Solid, 2, RGB(255, 255, 255));
  Pen:= SelectObject(DC, Pen);
  {$IFDEF WIN32}
  MoveToEx(DC, FBarX - 3, FBarY + FBarY1 + 3, NIL);
  {$ELSE}
  MoveTo(DC, FBarX - 3, FBarY + FBarY1 + 3);
  {$ENDIF}
  LineTo(DC, FBarX + FBarX1 + 3, FBarY + FBarY1 + 3);
  LineTo(DC, FBarX + FBarX1 + 3, FBarY - 3);
  Pen:= SelectObject(DC, Pen);
  DeleteObject(Pen);
  DrawBar;
  ReleaseDC(FWinHandle, DC);
end;


{Show the window. If the ParentHandle is non-zero, then the progress window
will be created as a child window of the specified window.}
procedure TAPIProgressBar.Execute(ParentHandle: THandle);
var
  C: array[0..128] of char;
begin
  {Get the handle of the icon.}
  FIconHandle:= 0;
  if not(FIcon.Empty) then FIconHandle:= FIcon.Handle
  else if FIconName <> '' then
    begin
      StrPCopy(C, FIconName);
      FIconHandle:= LoadIcon(HInstance, C);
    end;

  {Create the window.}
  FWinHandle:= CreateWindowEx(WS_EX_DLGMODALFRAME,
                              APIClassName, 'DelphiWindow',
                              WS_DLGFRAME,
                              0, 0, 
                              200, 100, ParentHandle, 0,
                              HInstance, nil);

  {Get rid of the title bar.}
  SetWindowLong(FWinHandle, GWL_STYLE,
                GetWindowLong(FWinHandle, GWL_STYLE) and not WS_CAPTION);
  {Add ourselves to the the list.}
  UsedWindows.Add(Self);
  {Force a window resize.}
  SendMessage(FWinHandle, wm_SizeAll, 0, 0);
  ShowWindow(FWinHandle, SW_SHOWNORMAL);
  UpdateWindow(FWinHandle);
end;

{Shutdown the window.}
procedure TAPIProgressBar.Halt;
begin
  {Unset the CurrentAPIProgressBar if necessary.}
  DestroyWindow(FWinHandle);
  UsedWindows.Remove(Self);
end;

{Paint procedure.}
procedure TAPIProgressBar.Paint;
var
  P: TPaintStruct;
begin
  {Start the paint.}
  BeginPaint(FWinHandle, P);
  DrawWindow;
  {Finish the paint process.}
  EndPaint(FWinHandle, P);
end;

{Write method for BarMar property.}
procedure TAPIProgressBar.SetBarMax(Value: LongInt);
begin
  if Value > 0 then FBarMax:= Value;
  DrawBar;
end;

{Write method for BarUsed property.}
procedure TAPIProgressBar.SetBarUsed(Value: LongInt);
begin
  if (Value > 0) and (Value <= FBarMax) then FBarUsed:= Value;
  DrawBar;
  DebugEx(5, 'TAPIProgressBar', 'Bar', 'Bar set to value %d of %d', [FBarUsed, FBarMax]);
  {The progress bar has been set to FBarUsed of FBarMax.}
end;

{Resize the controls.}
procedure TAPIProgressBar.SizeAll;
var
  DC          : THandle;
  F, F1       : HFont;
  LX, LY, X, Y: Integer;
  C           : array[0..512] of char;
  {$IFDEF WIN32}
  T           : TSize;
  {$ELSE}
  L           : LongInt;
  {$ENDIF}
begin
  DC:= GetWindowDC(FWinHandle);
  F:= CreateFont(FFontSize, 0, 0, 0, 0,
                 0, 0, 0,
                 1, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
                 DEFAULT_QUALITY, DEFAULT_PITCH,
                 'Arial');
  F1:= SelectObject(DC, F);
  StrPCopy(C, FTextString);
  {$IFDEF WIN32}
  GetTextExtentPoint32(DC, C, Length(TextString), T);
  LX:= T.cx;
  LY:= T.cy;
  {$ELSE}
  L:= GetTextExtent(DC, C, Length(TextString));
  LX:= (L mod 65536){ + FIcon.Width};
  LY:= Trunc(L div 65536);
  {$ENDIF}
  if FIconHandle <> 0 then inc(LX, GetSystemMetrics(SM_CXICON));
  if (FIconHandle <> 0) and (GetSystemMetrics(SM_CYICON) > LY) then
    LY:= GetSystemMetrics(SM_CYICON);
  inc(LX, XBorder * 4);
  inc(LY, YBorder);
  {Get rid of font.}
  SelectObject(DC, F1);
  DeleteObject(F);
  ReleaseDC(FWinHandle, DC);
  FBarX:= 10;
  FBarX1:= LX - Round(2.5 * FBarX);
  FBarY:= LY + 10;
  FBarY1:= 15;
  X:= (GetSystemMetrics(SM_CXSCREEN) - LX) div 2;
  Y:= (GetSystemMetrics(SM_CYSCREEN) - LY) div 2;
  SetWindowPos(FWinHandle, 0, X, Y, LX, LY + 40, 0);
end;

const
  {Class name assigned to the TMiniWin window.}
  MiniClassName = 'AlohaOi.Mini';

{Message handling procedure for the TMiniWin window.}
function MiniProc(Window: HWnd; Message, WParam: Word;
                     LParam: LongInt): LongInt;
                     {$IFDEF WIN32}
                     stdcall;
                     {$ENDIF}
                     export;
var
  C: TMiniWin;
  i: Integer;
begin
  Result:= DefWindowProc(Window, Message, WParam, LParam);
  {Locate the correct object.}
  for i:= 0 to MiniWindows.Count - 1 do
    if TMiniWin(MiniWindows[i]).FWinHandle = LongInt(Window) then
      begin
        C:= TMiniWin(MiniWindows[i]);
        Result:= C.HandleMessage(Message, WParam, LParam);
      end;
end;

{Create a window for the object and add it to the list.}
constructor TMiniWin.Create(AOwner: TComponent);
var
  C: TDataCaption;
begin
  inherited Create(AOwner);
  {Build the window.}
  C:= BuildCaption(1);
  FWinHandle:= CreateWindow(MiniClassName, C, 0, 0, 0, 50, 50, HWND_DESKTOP,
                            0, HInstance, NIL);
  MiniWindows.Add(Self);
end;

{Destroy the window and remove it from the list.}
destructor TMiniWin.Destroy;
begin
  DestroyWindow(FWinHandle);
  MiniWindows.Remove(Self);
  inherited Destroy;
end;

{Returns a caption built with the specified ID.}
function TMiniWin.BuildCaption(ID: Word): TDataCaption;
var
  i: Integer;
  S: String;
begin
  for i:= 0 to 128 do Result[i]:= #0;
  S:= 'Aloha Oi Software MINI: #' + IntToStr(ID);
  StrPCopy(Result, S);
end;

{Locates a unique ID. A caption will be built using an ID and the
BuildCaption function. If a window is found with that caption, then
another ID will be tried.}
function TMiniWin.FindUniqueID: Word;
var
  C: TDataCaption;
  i: Integer;
begin
  i:= 0;
  repeat
    inc(i);
    C:= BuildCaption(i);
  until FindWindow(NIL, C) = 0;
  ID:= i;
  Result:= i;
end;

{Handle messages.}
function TMiniWin.HandleMessage(Msg, WParam, LParam: LongInt): LongInt;
begin
  Result:= 0;
  if Assigned(FOnMessage) then Result:= FOnMessage(Self, Msg, WParam, LParam);
end;

{Write method for the ID property.}
procedure TMiniWin.SetID(Value: Word);
var
  C: TDataCaption;
begin
  FID:= Value;
  C:= BuildCaption(FID);
  SetWindowText(FWinHandle, C);
end;

{$IFNDEF WIN32}
const
  {WM_COPYDATA message definition for Delphi 1.x}
  WM_COPYDATA = 74;
{$ENDIF}

var
  HereMessage    : LongInt;
  RequestMessage : LongInt;
  ResponseMessage: LongInt;

constructor TCopyData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {Setup the message values.}
  HereMessage:= RegisterWindowMessage('Aloha.Oi.Software.CopyData.Here');
  RequestMessage:= RegisterWindowMessage('Aloha.Oi.Software.CopyData.Request');
  ResponseMessage:= RegisterWindowMessage('Aloha.Oi.Software.CopyData');
  {Setup.}
  FDebugMsg:= True;
  FTarget:= 0;
  FTargetID:= 1;
end;

{Returns a caption built with the specified ID.}
function TCopyData.BuildCaption(ID: Word): TDataCaption;
var
  i: Integer;
  S: String;
begin
  for i:= 0 to 128 do Result[i]:= #0;
  S:= 'Aloha Oi Software CopyData: #' + IntToStr(ID);
  StrPCopy(Result, S);
end;

{This method is called when a data request is made. In TCopyData, this
triggers the OnRequest event.}
function TCopyData.DataRequested(Data: LongInt): LongInt;
begin
  Result:= 0;
  if Assigned(FOnRequest) then Result:= FOnRequest(Self, Data);
end;

{Custom message handler for the TCopyData object.}
function TCopyData.HandleMessage(Msg, WParam, LParam: LongInt): LongInt;
begin
  Result:= inherited HandleMessage(Msg, WParam, LParam);
  if Msg = WM_COPYDATA then
    Result:= ReceiveData(PCopyDataStruct(LParam))
  else if (Msg = ResponseMessage) and (WParam = ID) then Result:= ID
  {Handle any data requests.}
  else if Msg = RequestMessage then
    begin
      FTarget:= LParam;
      Result:= DataRequested(WParam);
    end;
end;

{Method that first processes the data. In TCopyData, this does nothing.
This is good place to handle incoming data in descendant components}
function TCopyData.IncomingData(Data: LongInt; DataPointer: Pointer; SizeOfData: LongInt): LongInt;
begin
  Result:= 0;
end;

{Attempt to find the TCopyData object that has the same ID as the TargetID
property. If the target is found, the method will return True.}
function TCopyData.LocateTarget: Boolean;
var
  C: TDataCaption;
begin
  if FDebugMsg then
    DebugEx(2, 'TCopyData', 'Connection', '%d Location %d', [FID, FTargetID]);
  FTarget:= 0;
  C:= BuildCaption(FTargetID);
  FTarget:= FindWindow(NIL, C);
  {If a handle was found, then ping the window to see if it will respond.}
  if FTarget <> 0 then
    begin
      if SendMessage(FTarget, ResponseMessage, FTargetID, 0) <> FTargetID then FTarget:= 0;
    end;
  Result:= (FTarget <> 0);
  if FDebugMsg then
    DebugEx(3, 'TCopyData', 'Connection', 'Target Located: %s (%d)', [BoolToStr(Result), FTargetID]);
end;

{Reads a data buffer into a TMemoryStream.}
procedure TCopyData.ReadStream(Stream: TMemoryStream; DataPointer: Pointer; Size: LongInt);
begin
  Stream.Clear;
  if (DataPointer <> NIL) and (Size <> 0) then
    begin
      Stream.SetSize(Size);
      Move(DataPointer^, Stream.Memory^, Size);
      Stream.Seek(0, 0);
    end;
end;

{Method to process a PCopyDataStruct object. This method triggers the
OnIncoming event. The value returned by the method will be returned to
the TCopyData that called this method. This method should not be called
by the user; it is called by the message handler.}
function TCopyData.ReceiveData(Data: PCopyDataStruct): LongInt;
begin
  if FDebugMsg then
    DebugEx(2, 'TCopyData', 'Connection', 'Received Data (%d)', [FID]);
  Result:= IncomingData(Data^.dwData, Data^.lpData, Data^.cbData);
  if Assigned(FOnIncoming) then
    Result:= FOnIncoming(Self, Data^.dwData, Data^.lpData, Data^.cbData);
  FGotStuff:= True;
end;

{Request data from the remote TCopyData object. The Wait parameter indicates
whether or not the method should wait for the first response back or exit
immediately.}
procedure TCopyData.RequestData(Request: Word; Wait: Boolean);
var
  Go: Boolean;
begin
  if FDebugMsg then
    DebugEx(2, 'TCopyData', 'Connection', '%d requesting from %d (%d)',
            [FID, FTargetID, Request]);
  if not(StillLinked) then EXIT;
  FGotStuff:= False;
  PostMessage(FTarget, RequestMessage, Request, FWinHandle);
  repeat
    Go:= YieldProcess or FGotStuff;
  until not(Wait) or Go;
end;

{This method will check to see if the TCopyData object is still linked to the
remote object. If the link still exists, then the method will return True.}
function TCopyData.StillLinked: Boolean;
begin
  Result:= (SendMessage(FTarget, ResponseMessage, FTargetID, 0) = FTargetID);
  if FDebugMsg then
    DebugEx(4, 'TCopyData', 'Connection', '%d linked to %d (%s)',
            [FID, FTargetID, BoolToStr(Result)]);
end;

{This method will send the data to the Target window via the WM_COPYDATA
message.}
function TCopyData.SendData(Data: LongInt; DataPointer: Pointer; SizeOfData: LongInt): LongInt;
var
  C: TCopyDataStruct;
begin
  if FDebugMsg then
    DebugEx(2, 'TCopyData', 'Connection', '%d sending to %d (%d)',
            [FID, FTargetID, Data]);
  {Build the structure.}
  C.dwData:= Data;
  C.lpData:= DataPointer;
  if DataPointer = NIL then C.cbData:= 0
  else C.cbData:= SizeOfData;
  {Send the message off.}
  Result:= SendMessage(FTarget, WM_COPYDATA, FWinHandle, LongInt(@C));
end;

{Sends a TMemoryStream to the remote TCopyData.}
procedure TCopyData.WriteStream(Data: LongInt; Stream: TMemoryStream);
begin
  SendData(Data, Stream.Memory, Stream.Size);
  Stream.Seek(0, 0);
end;

{Create the object. To create an initialize with a template, sue the
CreateEx constructor.}
constructor TDocumentTemplate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDocument:= TStringList.Create;
  FTemplate:= TStringList.Create;
end;

{Create and intialize the document template with a file name.
The TemplateName specifies a file that will be
used to initialize the Template property. If no name is specified, or
if the file doesn't exist, then the Template property will simply be
empty.}
constructor TDocumentTemplate.CreateEx(AOwner: TComponent; TemplateName: String);
begin
  Create(AOwner);
  {See if the template exists.}
  if FileExists(TemplateName) then
    begin
      FTemplate.LoadFromFile(TemplateName);
      FDocument.Assign(FTemplate);
    end;
end;

destructor TDocumentTemplate.Destroy;
begin
  FDocument.Free;
  FTemplate.Free;
  inherited Destroy;
end;

{Load a new template from a file. The Template and Document properties will
both be initialized to the contents of the file.}
procedure TDocumentTemplate.LoadTemplate(TemplateName: String);
begin
  FDocument.Clear;
  FTemplate.Clear;
  if FileExists(TemplateName) then
    begin
      FTemplate.LoadFromFile(TemplateName);
      FDocument.Assign(FTemplate);
    end;
end;

{Replace the specified Search text with the Replace text where ever is is
found in the Document property. If the CaseSensitive parameter is enabled
then an exact case match will be required before a replacement is made. The
method returns the number of replacements made.}
function TDocumentTemplate.ReplaceText(Search, Replace: String; CaseSensitive: Boolean): LongInt;
var
  i: Integer;
  j: Integer;
  S: String;
  T: String;
  Z: String;
begin
  Result:= 0;
  i:= 0;
  if not(CaseSensitive) then Search:= AnsiUpperCase(Search);
  while i < FDocument.Count do
    begin
      S:= FDocument[i];
      if not(CaseSensitive) then S:= AnsiUpperCase(FDocument[i]);
      j:= Pos(Search, S);
      while j > 0 do
        begin
          T:= FDocument[i];
          Z:= '';
          if j > 1 then Z:= Copy(T, 1, j - 1);
          Z:= Z + Replace;
          Z:= Z + Copy(T, j + Length(Search), Length(T));
          FDocument[i]:= Z;
          inc(Result);
          {Update.}
          S:= FDocument[i];
          if not(CaseSensitive) then S:= AnsiUpperCase(FDocument[i]);
          j:= Pos(Search, S);
        end;
      inc(i);
    end;
end;

{Write method for the Template property. Will make a copy of the Template
in the Document property.}
procedure TDocumentTemplate.SetTemplate(Value: TStringList);
begin
  FDocument.Assign(Value);
  FTemplate.Assign(Value);
end;

{Write method for the TemplateFile property. If the file exists, and not
in design mode, then load the file.}
procedure TDocumentTemplate.SetTemplateFile(Value: String);
begin
  FTemplateFile:= Value;
  if FileExists(FTemplateFile) then
    begin
      FTemplate.LoadFromFile(FTemplateFile);
      FDocument.Assign(FTemplate);
    end;
end;

{Add a backslash to a string.}
function AddBackSlash(const S: String): String;
begin
  Result:= S;
  if (Result <> '') and (Result[Length(S)] <> '\') then Result:= Result + '\';
end;

{Converts a boolean value into a string.}
function BoolToStr(B: Boolean): String;
begin
  if B then Result:= 'TRUE'
  else Result:= 'FALSE';
end;

{Compare two version strings if A is greater than B, the return value will
be -1. If A is less than B, the return value will be 1. If A and B are
equal, the return value will be 0.}
function CompareVersions(A, B: String): Integer;
begin
  Result:= 0;
  if A > B then Result:= -1
  else if A < B then Result:= 1;
end;

{Copy a file from Source to Dest, while popping Event everytime
a block is copied.  If Splice is enabled, then add the source file
onto any existing destination file.}
procedure CopyFile(Source, Dest: String; Event: TSizedNotify; Splice: Boolean);
begin
  CopySegment(Source, Dest, 0, GetFileSize(Source) - 1,
              51200, Splice, Event);
end;

{Copy a file from Source (from Start to Stop) to Dest, while popping
Event everytime a block of size BlockSize is copied. If Splice is enabled,
then the segment will be copied to the end of the Dest file.
Only the specified segment will be copied.}
procedure CopySegment(Source, Dest: String; Start, Stop, BlockSize: LongInt;
                      Splice: Boolean; Event: TSizedNotify);
var
  IHandle, OHandle: Integer;
  IStream, OStream: THandleStream;
  Block           : Pointer;
  SizeOfBlock     : LongInt;
begin
  {Raise exeception if file doesn't exist.}
  if not(FileExists(Source)) then
    raise EInOutError.Create('Source file doesn''t exist:' + Source);
  {Get the file buffer.}
  GetMem(Block, BlockSize);
  try
    {Get a handle to the source file.}
    IHandle:= FileOpen(Source, fmOpenRead or fmShareDenyNone);
    if IHandle > 0 then
      try
        {Create a stream to the input file.}
        IStream:= THandleStream.Create(IHandle);
        try
          {See if we should re-create the file or just open to add on
          to it.}
          if (not(Splice) and FileExists(Dest)) or
             not(FileExists(Dest)) then
            begin
              SysUtils.DeleteFile(Dest);
              OHandle:= FileCreate(Dest)
            end
          else OHandle:= FileOpen(Dest, fmOpenReadWrite);
          if OHandle > 0 then
            try
              {Create a stream to the output file.}
              OStream:= THandleStream.Create(OHandle);
              {If we are splicing, then jump to the correct spot.}
              if Splice then OStream.Seek(0, 2);
              try
                {Check segment locations.}
                if IStream.Size < Stop then
                  begin
                    MessageBox(0, 'BAD', 'TSETUP', MB_OK);
                    raise EInOutError.Create('Invalid stop position.');
                  end;
                if Stop < Start then
                  raise EInOutError.Create('Invalid start position.');
                {Jump to the correct spot in the input file.}
                IStream.Seek(Start, 0);
                {Go into a copy loop.}
                repeat
                  {Determine the size of the block to copy.}
                  SizeOfBlock:= Stop - Start + 1;
                  if BlockSize < SizeOfBlock then SizeOfBlock:= BlockSize;
                  {Read in, write out.}
                  IStream.Read(Block^, SizeOfBlock);
                  OStream.Write(Block^, SizeOfBlock);
                  {Advance the counter and release a few CPU cycles.}
                  inc(Start, SizeOfBlock);
                  YieldProcess;
                  {Fire off the event.}
                  if Assigned(Event) then Event(NIL, SizeOfBlock);
                until Start >= Stop;
                {Set the destination file's date to the source file's.}
                FileSetDate(OHandle, FileAge(Source));
              finally
                {Free dynamic memory.}
                OStream.Free;
              end;
            finally
              {Free output file.}
              FileClose(OHandle);
            end;
        finally
          {Free dynamic memory.}
          IStream.Free;
        end;
      finally
        {Free the input file.}
        FileClose(IHandle);
      end
    else raise EInOutError.Create('Unable to open source file.');
  finally
    {Release dynamic memory.}
    FreeMem(Block, BlockSize);
  end;
end;

{This function is provided for backwards compatibility only. The
DebugEx procedure should be used instead.}
procedure Debug(Level: Byte; Module, Area: LongInt; Msg: String);
begin
  DebugEx(Level, IntToStr(Module), IntToStr(Area), Msg, [NIL]);
end;

{This function will send out a debug message if a debugger is currently
enabled and linked to this program.}
procedure DebugEx(Level: Byte; Module, Area, Template: String; const Args: array of const);
var
  M: TMessageStream;
begin
  if (DebugPort = NIL) or not(DebugPort.StillLinked) then EXIT;
  M:= TMessageStream.Create;
  try
    {Build the debug message.}
    M.WriteInString(DebugID);
    M.WriteInDateTime(Now);
    M.WriteInInteger(Level);
    M.WriteInString(Module);
    M.WriteInString(Area);
    M.WriteInString(Format(Template, Args));
    {Transmit the debug message.}
    DebugPort.SendData(0, M.Memory, M.Size);
  finally
    M.Free;
  end;
end;

{Ensure that the link to the debugger is in place. If the link is down, an
attempt to reestablish it will be made. The return value indicates whether
or not the link is up when the method completes.}
function DebugRelink: Boolean;
begin
  Result:= DebugPort.StillLinked;
  if not(Result) then Result:= DebugPort.LocateTarget;
end;

{Delete the specified directory. If specified, also delete any subdirectories
using recursive calls. If the complete directory (and any subdirectories if
specified) is successful, or if the deletion is canceled via the DirNotify
event, the return value will be True. Otherwise, it will be False. If a file
in the directory cannot be deleted immediately or if an attempt to delete a
subdirectory fails, the method will also return False. Any files that cannot
be deleted will be scheduled for deletion when the system is rebooted.}
function DeleteDirectory(Dir: String; IncludeSubDirs: Boolean;
                         FileNotify, DirNotify: TDeletionNotify;
                         var RebootRequired: Boolean): Boolean;
var
  D: String;
  G: Boolean;
  i: Integer;
  L: TStringList;
begin
  Result:= True;
  D:= StripBackSlash(Dir);
  if not(DirExists(Dir)) then EXIT;
  G:= True;
  {If the directory is not a TSetup directory, then trigger the DirNotify event.
  If the return value from that is false, then abort the directory deletion.}
  if Assigned(DirNotify) then
    begin
      DirNotify(D, G);
      if not(G) then EXIT;
    end;
  L:= TStringList.Create;
  try
    {See if subdirectories should be deleted.}
    if IncludeSubDirs then
      begin
        GetSubdirectories(D, L);
        for i:= 0 to L.Count - 1 do
          if not(DeleteDirectory(L[i], True, FileNotify, DirNotify, RebootRequired)) then
            Result:= False;
      end;
    {Get a list of files in the directory.}
    GetFiles(D, '*.*', L);
    L.Sort;
    {Remove each file.}
    for i:= 0 to L.Count - 1 do
      begin
        {See if we should delete the file.}
        G:= True;
        {Trigger the event if necessary.}
        if Assigned(FileNotify) then FileNotify(L[i], G);
        if G then
          begin
            {Attempt to delete the file.}
            G:= SysUtils.DeleteFile(L[i]);
            if not(G) then
              begin
                {Unable to delete the file. Deleting the directory
                will also fail - so the function must return False.}
                Result:= False;
                RebootRequired:= True;
              end;
          end;
      end;
    if Result then Result:= DeleteJustDirectory(D);
  finally
    L.Free;
  end;
end;

{This will attempt to delete just the specified directory. No files contained
in the directory or any subdirectories beneath the directory will be removed.
The return value indicates if the deletion was successful.}
function DeleteJustDirectory(Dir: String): Boolean;
begin
  Result:= True;
  {Jump to a neutral directory.}
  try
    ChDir('C:\');
  except
    on Exception do; {Absorb the error.}
  end;
  {Attempt to delete the directory.}
  try
    RmDir(StripBackSlash(Dir));
  except
    on Exception do Result:= False;
  end;
end;

{This will see if a directory exists without using the FileCtrl unit.}
function DirExists(Dir: String): Boolean;
var
  OldDir: String;
begin
  {$I-}
  GetDir(0, OldDir);
  ChDir(Dir);
  Result:= IOResult = 0;
  ChDir(OldDir);
  {$I+}
end;

{Format into a Windows 3.1 compatible string. Windows 3.1 requires that the
root directory always have a trailing slash, while all other directories
do not have a trailing slash.}
function DirString(Dir: String): String;
var
  S: String;
begin
  S:= StripBackSlash(Dir);
  if Length(S) = 2 then S:= AddBackSlash(S);
  Result:= S;
end;

{This function will extract the root drive if the path is a standard drive
and path, or the computer name and volume name if the path is a UNC path.
If the AbsoluteDir is not an absolute path, then an empty string will be
returned.}
function ExtractRoot(AbsoluteDir: String): String;
var
  D: TStringList;
  P: PChar;
begin
  if IsRelativePath(AbsoluteDir) then Result:= ''
  else
    begin
      D:= TStringList.Create;
      try
        while Pos('\', AbsoluteDir) > 0 do
          AbsoluteDir[Pos('\', AbsoluteDir)]:= #13;
        P:= StrAlloc(Length(AbsoluteDir) + 1);
        StrPCopy(P, AbsoluteDir);
        D.SetText(P);
        StrDispose(P);
        {See if the path is a drive path or a UNC path.}
        if (Length(D[0]) = 2) and (D[0][2] = ':') then
          Result:= AddBackSlash(D[0])
        else if D.Count >=4 then
          Result:= '\\' + D[2] + '\' + D[3] + '\'
        else Result:= '';
      finally
        D.Free;
      end;
    end;
end;

{This will force the creation of the entire directory string. This is the
equivalent to ForceDirectories function in the FileCtrl unit.}
procedure ForceCreateDirectories(Dir: string);
begin
  Dir:= StripBackSlash(Dir);
  if (Length(Dir) < 3) or DirExists(Dir) then EXIT;
  ForceCreateDirectories(ExtractFilePath(Dir));
  {$I-}
  MkDir(Dir);
  if IOResult <> 0 then EXIT;
  {$I+}
end;

{$I+}
{Procedure to force the removal of an entire directory path.  It
will fail when one of the directories is non empty. No files in the
directories or any subdirectories are deleted.}
procedure ForceRemoveDirectories(Dir: String);
var
  S: String;
begin
  {Get us to the root so we don't get caught in
  a Win95 no-check scenario.}
  ChDir('\');
  {Initialize start dir.}
  S:= Dir;
  repeat
    {Remove a directory...}
    RmDir(StripBackSlash(S));
    {...locate the last directory...}
    repeat
      Delete(S, Length(S), 1);
    until (Length(S) = 3) or (S[Length(S)] = '\');
    {...then strip it from the path.}
    Delete(S, Length(S), 1);
  until Length(S) <= 3;
end;

{Returns the value of the DOS environment variable passed in EnvVar.
Note: EnvVar must be 253 chars or less, or it will be truncated to 253.}
function GetEnvVar(EnvVar: String): String;
var
  {$IFDEF WIN32}
  S   : array[0..512] of char;
  T   : array[0..50] of char;
  {$ELSE}
  Done: Boolean;
  i   : Integer;
  P   : PChar;
  S   : String;
  Vars: TStringList;
  {$ENDIF}
begin
  EnvVar:= AnsiUpperCase(EnvVar);
  {$IFDEF WIN32}
  StrPCopy(T, EnvVar);
  {FIXUP What if variable is > 512 chars?}
  GetEnvironmentVariable(T, S, 512);
  Result:= StrPas(S);
  {$ELSE}
  Result:= '';
  P:= GetDOSEnvironment;
  {Format the variable.}
  EnvVar:= EnvVar + '=';
  S:= '';
  Done:= False;
  Vars:= TStringList.Create;
  try
    {Cycle through the vars, grabbing each one.}
    while P^ <> #0 do
      begin
        S:= StrPas(P);
        Vars.Add(S);
        inc(P, Length(S) + 1);
      end;
    {Cycle through the ones we found, looking for the specified one.}
    for i:= 0 to Vars.Count - 1 do
      if Pos(EnvVar, AnsiUpperCase(Vars[i])) = 1 then
        begin
          Result:= Copy(Vars[i], Length(EnvVar) + 1, Length(Vars[i]));
          BREAK;
        end;
  finally
    {Free dynamic stuff.}
    Vars.Free;
  end;
  {$ENDIF}
end;

{Returns a list of files (with fully expanded drive, path, and file names)
specified by the Dir parameter.}
procedure GetFiles(Dir, FileMask: String; FileList: TStrings);
var
  i: Integer;
  S: TSearchRec;
begin
  FileList.Clear;
  {Run through the directory retrieving any files.}
  i:= FindFirst(AddBackSlash(Dir) + FileMask, faAnyFile, S);
  while i = 0 do
    begin
      if ((S.Attr and faDirectory) = 0) and ((S.Attr and faVolumeID) = 0) then
        FileList.Add(AddBackSlash(Dir) + S.Name);
      i:= FindNext(S);
    end;
  SysUtils.FindClose(S);
end;

{Get the size of a file. If the return value is -1, then an error occured
while opening the file.}
function GetFileSize(FileName: String): LongInt;
var
  F: TFileStream;
begin
  try
    F:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    Result:= F.Size;
    F.Free;
  except
    on Exception do Result:= -1;
  end;
end;

{Returns the file version of the specified file. If no file version is stored
in the file, the return version will be '0000.0000.0000.0000'}
function GetFileVersion(FileName: String): String;
var
  {$IFDEF WIN32}
  Buffer           : PVSFixedFileInfo;
  {$ELSE}
  Buffer           : PVS_FixedFileInfo;
  {$ENDIF}
  BufferP          : Pointer;
  Handle           : {$IFDEF WIN32} DWord; {$ELSE} LongInt; {$ENDIF}
  P                : Pointer;
  PFileName        : PChar;
  Size             : {$IFDEF WIN32} DWord; {$ELSE} LongInt; {$ENDIF}
  Size2            : {$IFDEF WIN32}
                       {$IFDEF HIGHDELPHI}LongWord;{$ELSE}Integer;{$ENDIF}
                     {$ELSE} Word; {$ENDIF}
begin
  FileName:= ParamStr(0);
  Result:= '0000.0000.0000.0000';
  {Get the pchar of the file name.}
  PFileName:= StrAlloc(Length(FileName) + 1);
  try
    StrPCopy(PFileName, FileName);
    Size:= GetFileVersionInfoSize(PFileName, Handle);
    if Size > 0 then
      begin
        GetMem(P, Size);
        try
          GetFileVersionInfo(PFileName, Handle, Size, P);
          VerQueryValue(P, '\', BufferP, Size2);
          Buffer:= BufferP;
          Result:= Format('%.4d.%.4d.%.4d.%.4d',
                          [HiWord(Buffer^.dwFileVersionMS),
                           LoWord(Buffer^.dwFileVersionMS),
                           HiWord(Buffer^.dwFileVersionLS),
                           LoWord(Buffer^.dwFileVersionLS)]);
        finally
          FreeMem(P, Size);
        end;
      end;
  finally
    StrDispose(PFileName);
  end;
end;

{This method will return a list of subdirectories that are found under the
specified directory. Each subdirectory listed will include the full path:
drive and path.}
procedure GetSubdirectories(Dir: String; var SubDirs: TStringList);
var
  D: String;
  i: Integer;
  S: TSearchRec;
begin
  D:= StripBackSlash(Dir);
  SubDirs.Clear;
  i:= FindFirst(Dir + '\*.*', faDirectory, S);
  while i = 0 do
    begin
      if ((S.Attr and faDirectory) = faDirectory) and (S.Name <> '.') and
         (S.Name <> '..') then SubDirs.Add(Dir + '\' + S.Name);
      i:= FindNext(S);
    end;
  SysUtils.FindClose(S);
end;

{This method will return a unique file name that can exist within the
directory specified by the Dir parameter. The FileTemplate and
ExtensionTemplate parameters provide templates that will be used for either
the file name or the extension. If the FileTemplate is not empty, then
the extension will be varied to create a unique name. If the FileTemplate
is empty, but the extension is not, then the file name will be varied to
create a unique file name.  If both template parameters are specified, then
the extension will be varied to provide a unique file name. If neither
template parameter is specified, then the file name will be varied with the
extension "TMP" to create a unique file name. If the specified directory
does not exist, then an exception is raised. If a unique file name using
these algorithms cannot be found, then an exception will be raised. The
ExtensionTemplate parameter should NOT include the period. All template
names should conform to the 8.3 format to ensure compatibility with Windows 3.1,
although no checks are made to enforce this.}
function GetUniqueFileName(Dir, FileTemplate, ExtensionTemplate: String): String;
var
  D: String;
  L: LongInt;
  S: String;
  V: Integer;
  function IsUnique(S: String): Boolean;
  begin
    Result:= not(FileExists(S));
  end;
begin
  {Make sure the directory exists.}
  D:= AddBackSlash(Dir);
  if not(DirExists(D)) then
    raise Exception.Create('Directory does not exist: ' + D);
  {See what part should be varied. V=1 means vary the name, V=0 means
  vary the extension.}
  V:= 0;
  if (FileTemplate = '') or
     ((FileTemplate <> '') and (ExtensionTemplate <> '')) then V:= 1;
  if (V = 0) and (ExtensionTemplate = '') then ExtensionTemplate:= 'TMP'; 
  {Go into a loop for all variations.}
  L:= 0;
  repeat
    {See if the algorithm has been maxed out.}
    try
      case V of
        0: if L > 999 then raise Exception.Create('');
        1: if L > 99999999 then raise Exception.Create('');
      end;
    except
      on Exception do
        raise Exception.Create('Unable to find unique file name');
    end;
    {Build the file name.}
    case V of
      0: S:= Format('%s%s.%.3d', [D, FileTemplate, L]);
      1: S:= Format('%s%.8d.%s', [D, L, ExtensionTemplate]);
    end;
    {Move onto the next item.}
    inc(L);
  until IsUnique(S);
  Result:= S;
end;

{This function will return True if the specified path or file name is an
absolute path or file name. To be absolute, the path/file name must either
specify a drive letter or use the UNC convention of \\ to indicate a computer
name. By this definition, a relative path is anything that is not an
absolute path.}
function IsAbsolutePath(Dir: String): Boolean;
var
  K: set of Char;
begin
  K:= ['a'..'z', 'A'..'Z'];
  if Length(Dir) < 3 then Result:= False
  else
    begin
      if (Dir[1] in K) and (Dir[2] = ':') and (Dir[3] = '\') then Result:= True
      else if (Dir[1] = '\') and (Dir[2] = '\') then Result:= True
      else Result:= False;
    end;
end;

{This function simply returns the opposite value of the IsAbsolutePath
function.  See IsAbsolutePath for more details on what a relative path is.}
function IsRelativePath(Dir: String): Boolean;
begin
  Result:= not(IsAbsolutePath(Dir));
end;

{This will return True if the particular byte mask A is present in Attributes.}
function IsSet(A, Attributes: LongInt): Boolean;
begin
  Result:= (Attributes and A) = A;
end;

{This function will return an absolute path or file name from the
RelativeTarget parameter and based upon the BaseDir path. The RelativeTarget
parmater must be a relative path or file name while the BaseDir must
be an absolute path (see IsAbsolutePath for more details on absolute and
relative). If this criteria is not met, then an empty string will be returned.
An empty string will also be returned if RelativeTarget in invalid in
relation to BaseDir.}
function MakeAbsolute(BaseDir, RelativeTarget: String): String;
var
  i: Integer;
  P: PChar;
  T: TStringList;
begin
  Result:= '';
  {Check BaseDir and RelativeTarget.}
  if IsRelativePath(BaseDir) or IsAbsolutePath(RelativeTarget) then EXIT
  else
    begin
      T:= TStringList.Create;
      try
        {Decompose the paths.}
        BaseDir:= AddBackSlash(BaseDir);
        RelativeTarget:= StripBackSlash(RelativeTarget);
        while Pos('\', RelativeTarget) > 0 do
          RelativeTarget[Pos('\', RelativeTarget)]:= #13;
        P:= StrAlloc(Length(RelativeTarget) + 1);
        StrPCopy(P, RelativeTarget);
        T.SetText(P);
        StrDispose(P);
        {Walk down the relative path, adjust at each step.}
        Result:= BaseDir;
        for i:= 0 to T.Count - 1 do
          begin
            {See if the root directory is specifed in the RelativeTarget.}
            if (i = 0) and (T[0] = '') then Result:= ExtractRoot(BaseDir)
            else
              begin
                {Do the adjustment.}
                if T[i] = '..' then
                  begin
                    {Jump up a directory.}
                    Result:= ExtractFilePath(StripBackSlash(Result));
                    if Result = '' then EXIT;
                  end
                else Result:= AddBackSlash(Result) + T[i];
              end;
          end;
      finally
        T.Free;
      end;
    end;
end;

{This function will return a path or file name that is relative
(see IsAbsolutePath for details on absolute and relative) using the
BaseDir parameter as base path. The FullTarget parameter must be either
absolute path name or file name. The resulting path or file name will
be either simply truncated to make a relative path or up-directory changes
('..') will be inserted to ensure that FullTarget will be relative in terms
of BaseDir.  If either the BaseDir or the FullTarget is not an absolute
path, the return value will be the empty string.}
function MakeRelative(BaseDir, FullTarget: String): String;
var
  B: TStringList;
  F: TStringList;
  i: Integer;
  j: Integer;
  P: PChar;
begin
  Result:= '';
  {Make sure that both the BaseDir and FullTarget are absolute.}
  if IsRelativePath(BaseDir) or IsRelativePath(FullTarget) then EXIT
  else
    begin
      B:= TStringList.Create;
      F:= TStringList.Create;
      try
        {Decompose the paths.}
        BaseDir:= StripBackSlash(BaseDir);
        FullTarget:= StripBackSlash(FullTarget);
        while Pos('\', BaseDir) > 0 do BaseDir[Pos('\', BaseDir)]:= #13;
        while Pos('\', FullTarget) > 0 do FullTarget[Pos('\', FullTarget)]:= #13;
        P:= StrAlloc(Max(Length(BaseDir), Length(FullTarget)) + 1);
        StrPCopy(P, BaseDir);
        B.SetText(P);
        StrPCopy(P, FullTarget);
        F.SetText(P);
        {Locate divergence point.}
        for i:= 0 to Min(B.Count, F.Count) do
          if AnsiUpperCase(B[i]) <> AnsiUpperCase(F[i]) then BREAK;
        {Walk up the base path if neccesary.}
        if i < B.Count then
          for j:= i to B.Count do Result:= Result + '..\';
        {Now walk down the target path if necessary.}
        if i < F.Count then
          for j:= i to F.Count do
            begin
              Result:= Result + F[j];
              if j <> F.Count then Result:= Result + '\';
            end;
      finally
        B.Free;
        F.Free;
      end;
    end;
end;

{Return the maximum value of A and B. Delphi32 already has this function;
this is mainly for Delphi 1.}
function Max(A, B: LongInt): LongInt;
begin
  if A > B then Result:= A
  else Result:= B;
end;

{Return the minimum value of A and B. Delphi32 already has this fucntion;
this is mainly for Delphi 1}
function Min(A, B: LongInt): LongInt;
begin
  if A < B then Result:= A
  else Result:= B;
end;

{This function will parse out the first string preceeding
the separator characters.}
function Parse(var S: String; Separator: String): String;
var
  i: Integer;
begin
  i:= Pos(Separator, S);
  if i <> 0 then
    begin
      Result:= Copy(S, 1, i - 1);
      Delete(S, 1, i + Length(Separator) - 1);
    end
  else Result:= S;
end;

{This will display a message box with the appropiate strings.}
function PMsgDlg(Msg, Caption: String; TextType: Word): Integer;
var
  C, M: PChar;
begin
  {See if we should overwrite the caption.}
  if Caption = '' then Caption:= ExtractFileName(ParamStr(0));
  {Allocate the strings.}
  C:= StrAlloc(Length(Caption) + 1);
  M:= StrAlloc(Length(Msg) + 1);
  try
    StrPCopy(C, Caption);
    StrPCopy(M, Msg);
    Result:= MessageBox(0, M, C, TextType or MB_TASKMODAL);
  finally
    {Free the strings.}
    StrDispose(C);
    StrDispose(M);
  end;
end;

{This will set a particular attribute mask A into Attributes based on the
value of Value.}
procedure SetAttribute(A: Longint; var Attributes: LongInt; Value: Boolean);
begin
  if Value then Attributes:= Attributes OR A
  else Attributes:= Attributes AND Not(A)
end;

{Removes trailing backslash from S, if one exists }
function StripBackSlash(const S: String): String;
begin
  Result:= S;
  if (Result <> '') and (Result[Length(Result)] = '\') then
    Delete(Result, Length(Result), 1);
end;

{This will strip out all occurances of #13#10 in a pchar, but will
only turn #13#10#13#10 into #13#10.}
function StripCRLF(P: PChar): PChar;
var
  D: Boolean;
  i: Integer;
  S: String;
begin
  i:= 0;
  D:= False;
  S:= '';
  while P[i] <> #0 do
    begin
      if (P[i] = #13) and (P[i + 1] = #10) and not(D) then
        begin
          inc(i, 2);
          D:= True
        end
      else
        begin
          D:= False;
          S:= S + P[i];
          inc(i);
        end;
    end;
  Result:= StrAlloc(Length(S) + 1);
  StrPCopy(Result, S);
end;

{Wrap the specified string in paranthesis.}
function Wrap(S: String): String;
begin
  Result:= '(' + S + ')';
end;

{This is essentially the same as Application.ProcessMessage, except that it
does not require either the Forms or Dialogs units.}
function YieldProcess: Boolean;
var
  msg: TMsg;
begin
  while PeekMessage(msg, 0, 0, 0, PM_REMOVE) do
    begin
      if msg.message = WM_QUIT then
        begin
          PostQuitMessage(msg.wParam);
          Result:= True;
          EXIT;
        end
      else
        begin
          TranslateMessage(msg);
          DispatchMessage(msg)
        end
    end;
  Result:= False;
end;

{== Unit intialization and cleanup routines.}
{$IFNDEF WIN32}
var
  OldExitProc: Pointer;
{$ENDIF}

procedure UnitCleanup;
{Cleanup the unit.}
begin
  {$IFNDEF WIN32}
  ExitProc:= OldExitProc;
  {$ENDIF}

  {Clear up debug stuff.}
  DebugEx(0, 'DEBUGGER', 'Status', 'PROGRAM STOP', [NIL]);
  DebugPort.Free;
  DebugPort:= NIL;

  MiniWindows.Free;
  UsedWindows.Free;
end;

procedure UnitStartup;
{Startup the unit.}
var
  A   : array[0..1024] of char;
begin
  {Create the base class for TAPIProgressBar.}
  with APIWinClass do
    begin
      style        := 0;
      lpfnWndProc  := @APIProgressBarProc;
      cbClsExtra   := 0;
      cbWndExtra   := 0;
      hInstance    := 0;
      hIcon        := 0;
      hCursor      := 0;
      hbrBackground:= 0;
      lpszMenuName := '';
      lpszClassName:= APIClassName;
    end;

  {Create the base class for the TMiniWin.}
  with MiniWinClass do
    begin
      style        := 0;
      lpfnWndProc  := @MiniProc;
      cbClsExtra   := 0;
      cbWndExtra   := 0;
      hInstance    := 0;
      hIcon        := 0;
      hCursor      := 0;
      hbrBackground:= 0;
      lpszMenuName := '';
      lpszClassName:= MiniClassName;
    end;

  {Make sure we aren't already running.}
  if HPrevInst = 0 then
    begin
      {Build the mini window class.}
      MiniWinClass.hInstance:= HInstance;
      {$IFDEF WIN32}
      Windows.RegisterClass(MiniWinClass);
      {$ELSE}
      WinProcs.RegisterClass(MiniWinClass);
      {$ENDIF}
      {Build the rest of the class...}
      APIWinClass.hInstance:= HInstance;
      APIWinClass.hCursor:= LoadCursor(0, idc_Arrow);
      APIWinClass.hbrBackground:= GetStockObject(LTGRAY_BRUSH);
      {...and create.}
      {$IFDEF WIN32}
      Windows.RegisterClass(APIWinClass);
      {$ELSE}
      WinProcs.RegisterClass(APIWinClass);
      {$ENDIF}
    end;

  {Create the list to hold all the windows.}
  MiniWindows:= TList.Create;
  UsedWindows:= TList.Create;

  {Assign the temporary path.}
  TemporaryPath:= StripBackSlash(GetEnvVar('TEMP'));
  if TemporaryPath = '' then TemporaryPath:= StripBackSlash(GetEnvVar('TMP'));
  if TemporaryPath = '' then TemporaryPath:= 'C:\';
  TemporaryPath:= AddBackSlash(TemporaryPath);

  {Assign the Windows path.}
  GetWindowsDirectory(A, 255);
  WindowsPath:= AddBackSlash(StrPas(A));

  {Assign the Windows System path.}
  GetSystemDirectory(A, 255);
  WindowsSystemPath:= AddBackSlash(StrPas(A));

  {Create necessary debug stuff.}
  GetModuleFileName(HInstance, A, 1024);
  DebugID:= ExtractFileName(StrPas(A));
  DebugPort:= TCopyData.Create(NIL);
  DebugPort.FDebugMsg:= False;
  DebugPort.ID:= 10101;
  DebugPort.TargetID:= 7771;
  DebugPort.LocateTarget;
  if DebugPort.StillLinked then
    DebugEx(0, 'DEBUGGER', 'Status', 'Debugger located and being used.', [NIL]);
  DebugEx(0, 'DEBUGGER', 'Status', 'PROGRAM START', [NIL]);
end;

initialization
  {$IFNDEF WIN32}
  OldExitProc:= ExitProc;
  ExitProc:= @UnitCleanup;
  {$ENDIF}
  UnitStartup;

{$IFDEF WIN32}
finalization
  UnitCleanup;
{$ENDIF}


end.

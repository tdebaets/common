{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{     Copyright (c) 1985-1999, Microsoft Corporation    }
{                                                       }
{     Translation Copyright (c) 1995-2008 CodeGear      }
{                   Mike Lischke                        }
{                                                       }
{*******************************************************}

{*******************************************************}
{       Visual Styles (Themes) API interface unit       }
{*******************************************************}

unit UxTheme;

interface

uses
  Windows, CommCtrl;

procedure FreeThemeLibrary;
function InitThemeLibrary: Boolean;
function UseThemes: Boolean;

const
  WM_THEMECHANGED = $031A;

type
  HTHEME = THANDLE;

var
  OpenThemeData: function(hwnd: HWND; pszClassList: LPCWSTR): HTHEME; stdcall;
  CloseThemeData: function(hTheme: HTHEME): HRESULT; stdcall;
  DrawThemeBackground: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer;
    const pRect: TRect; pClipRect: PRECT): HRESULT; stdcall;
  DrawThemeText: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer;
    pszText: LPCWSTR; iCharCount: Integer; dwTextFlags, dwTextFlags2: DWORD;
    const pRect: TRect): HRESULT; stdcall;
  GetThemeBackgroundContentRect: function(hTheme: HTHEME; hdc: HDC;
    iPartId, iStateId: Integer; const pBoundingRect: TRect;
    pContentRect: PRECT): HRESULT; stdcall;
  GetThemeBackgroundExtent: function(hTheme: HTHEME; hdc: HDC;
    iPartId, iStateId: Integer; const pContentRect: TRect;
    var pExtentRect: TRect): HRESULT; stdcall;

const
  DTT_GRAYED = $1;
  { For Windows >= Vista }
  DTT_FLAGS2VALIDBITS = DTT_GRAYED;

type
  THEMESIZE = (
    TS_MIN,             // minimum size
    TS_TRUE,            // size without stretching
    TS_DRAW             // size that theme mgr will use to draw part
  );
  TThemeSize = THEMESIZE;

var
  GetThemePartSize: function(hTheme: HTHEME; hdc: HDC;
    iPartId, iStateId: Integer; prc: PRECT; eSize: THEMESIZE;
    var psz: TSize): HRESULT; stdcall;
  GetThemeTextExtent: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; pszText: LPCWSTR;
    iCharCount: Integer; dwTextFlags: DWORD; pBoundingRect: PRECT; var pExtentRect: TRect): HRESULT; stdcall;
  GetThemeTextMetrics: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer;
    var ptm: TEXTMETRICW): HRESULT; stdcall;
  GetThemeBackgroundRegion: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pRect: TRect;
    var pRegion: HRGN): HRESULT; stdcall;

{ HitTestThemeBackground, HitTestThemeBackgroundRegion flags }
const
  HTTB_BACKGROUNDSEG         = $0000;
  HTTB_FIXEDBORDER           = $0002;  // Return code may be either HTCLIENT or HTBORDER.
  HTTB_CAPTION               = $0004;
  HTTB_RESIZINGBORDER_LEFT   = $0010; // Hit test left resizing border,
  HTTB_RESIZINGBORDER_TOP    = $0020; // Hit test top resizing border
  HTTB_RESIZINGBORDER_RIGHT  = $0040; // Hit test right resizing border
  HTTB_RESIZINGBORDER_BOTTOM = $0080; // Hit test bottom resizing border
  HTTB_RESIZINGBORDER        = (HTTB_RESIZINGBORDER_LEFT or HTTB_RESIZINGBORDER_TOP or
                                HTTB_RESIZINGBORDER_RIGHT or HTTB_RESIZINGBORDER_BOTTOM);
  HTTB_SIZINGTEMPLATE        = $0100;
  HTTB_SYSTEMSIZINGMARGINS   = $0200;

var
  HitTestThemeBackground: function(hTheme: HTHEME; hdc: HDC;
    iPartId, iStateId: Integer; dwOptions: DWORD; const pRect: TRect;
    hrgn: HRGN; ptTest: TPoint; var pwHitTestCode: WORD): HRESULT; stdcall;
  DrawThemeEdge: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer;
    const pDestRect: TRect; uEdge, uFlags: UINT; pContentRect: PRECT): HRESULT; stdcall;
  DrawThemeIcon: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer;
    const pRect: TRect; himl: HIMAGELIST; iImageIndex: Integer): HRESULT; stdcall;
  IsThemePartDefined: function(hTheme: HTHEME; iPartId, iStateId: Integer): BOOL; stdcall;
  IsThemeBackgroundPartiallyTransparent: function(hTheme: HTHEME;
    iPartId, iStateId: Integer): BOOL; stdcall;
  GetThemeColor: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
    var pColor: COLORREF): HRESULT; stdcall;
  GetThemeMetric: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId, iPropId: Integer;
    var piVal: Integer): HRESULT; stdcall;
  GetThemeString: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
    pszBuff: LPWSTR; cchMaxBuffChars: Integer): HRESULT; stdcall;
  GetThemeBool: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
    var pfVal: BOOL): HRESULT; stdcall;
  GetThemeInt: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
    var piVal: Integer): HRESULT; stdcall;
  GetThemeEnumValue: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
    var piVal: Integer): HRESULT; stdcall;
  GetThemePosition: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
    var pPoint: TPoint): HRESULT; stdcall;
  GetThemeFont: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId, iPropId: Integer;
    var pFont: LOGFONTW): HRESULT; stdcall;
  GetThemeRect: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
    var pRect: TRect): HRESULT; stdcall;

type
  _MARGINS = record
    cxLeftWidth: Integer;      // width of left border that retains its size
    cxRightWidth: Integer;     // width of right border that retains its size
    cyTopHeight: Integer;      // height of top border that retains its size
    cyBottomHeight: Integer;   // height of bottom border that retains its size
  end;
  MARGINS = _MARGINS;
  PMARGINS = ^MARGINS;
  TMargins = MARGINS;

var
  GetThemeMargins: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId, iPropId: Integer; prc: PRECT;
    var pMargins: MARGINS): HRESULT; stdcall;

const
  MAX_INTLIST_COUNT = 10;
  MAX_INTLIST_COUNT_600 = 402; // For Windows >= 6.00 (Vista)

type
  _INTLIST = record
    iValueCount: Integer;      // number of values in iValues
    iValues: array [0..MAX_INTLIST_COUNT - 1] of Integer;
  end;
  INTLIST = _INTLIST;
  PINTLIST = ^INTLIST;
  TIntList = INTLIST;

var
  GetThemeIntList: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; var pIntList: INTLIST): HRESULT; stdcall;

type
  PROPERTYORIGIN = (
    PO_STATE,           // property was found in the state section
    PO_PART,            // property was found in the part section
    PO_CLASS,           // property was found in the class section
    PO_GLOBAL,          // property was found in [globals] section
    PO_NOTFOUND);       // property was not found
  TPropertyOrigin = PROPERTYORIGIN;

var
  GetThemePropertyOrigin: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
    var pOrigin: PROPERTYORIGIN): HRESULT; stdcall;
  SetWindowTheme: function(hwnd: HWND; pszSubAppName: LPCWSTR;
    pszSubIdList: LPCWSTR): HRESULT; stdcall;
  GetThemeFilename: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
    pszThemeFileName: LPWSTR; cchMaxBuffChars: Integer): HRESULT; stdcall;
  GetThemeSysColor: function(hTheme: HTHEME; iColorId: Integer): COLORREF; stdcall;
  GetThemeSysColorBrush: function(hTheme: HTHEME; iColorId: Integer): HBRUSH; stdcall;
  GetThemeSysBool: function(hTheme: HTHEME; iBoolId: Integer): BOOL; stdcall;
  GetThemeSysSize: function(hTheme: HTHEME; iSizeId: Integer): Integer; stdcall;
  GetThemeSysFont: function(hTheme: HTHEME; iFontId: Integer;
    var plf: LOGFONTW): HRESULT; stdcall;
  GetThemeSysString: function(hTheme: HTHEME; iStringId: Integer;
    pszStringBuff: LPWSTR; cchMaxStringChars: Integer): HRESULT; stdcall;
  GetThemeSysInt: function(hTheme: HTHEME; iIntId: Integer;
    var piValue: Integer): HRESULT; stdcall;
  IsThemeActive: function: BOOL; stdcall;
  IsAppThemed: function: BOOL; stdcall;
  GetWindowTheme: function(hwnd: HWND): HTHEME; stdcall;

const
  ETDT_DISABLE       = $00000001;
  ETDT_ENABLE        = $00000002;
  ETDT_USETABTEXTURE = $00000004;
  { For Windows >= Vista }
  ETDT_USEAEROWIZARDTABTEXTURE    = $00000008;

  ETDT_ENABLETAB     = (ETDT_ENABLE or ETDT_USETABTEXTURE);

  { For Windows >= Vista }
  ETDT_ENABLEAEROWIZARDTAB = (ETDT_ENABLE or ETDT_USEAEROWIZARDTABTEXTURE);
  ETDT_VALIDBITS           = ETDT_DISABLE or
                             ETDT_ENABLE or
                             ETDT_USETABTEXTURE or
                             ETDT_USEAEROWIZARDTABTEXTURE;

var
  EnableThemeDialogTexture: function(hwnd: HWND; dwFlags: DWORD): HRESULT; stdcall;

var
  IsThemeDialogTextureEnabled: function(hwnd: HWND): BOOL; stdcall;


{ Flags to control theming within an application }
const
  STAP_ALLOW_NONCLIENT   = (1 shl 0);
  STAP_ALLOW_CONTROLS    = (1 shl 1);
  STAP_ALLOW_WEBCONTENT  = (1 shl 2);
  { For Windows >= Vista }
  STAP_VALIDBITS         = STAP_ALLOW_NONCLIENT or
                           STAP_ALLOW_CONTROLS or
                           STAP_ALLOW_WEBCONTENT;


var
  GetThemeAppProperties: function: DWORD; stdcall;
  SetThemeAppProperties: procedure(dwFlags: DWORD); stdcall;
  GetCurrentThemeName: function(pszThemeFileName: LPWSTR;
    cchMaxNameChars: Integer; pszColorBuff: LPWSTR; cchMaxColorChars: Integer;
    pszSizeBuff: LPWSTR; cchMaxSizeChars: Integer): HRESULT; stdcall;


// The following defines were modified to fix a crash of the Delphi 4 compiler
// on incremental compilation.
const
  {SZ_THDOCPROP_DISPLAYNAME               = WideString('DisplayName');
  SZ_THDOCPROP_CANONICALNAME             = WideString('ThemeName');
  SZ_THDOCPROP_TOOLTIP                   = WideString('ToolTip');
  SZ_THDOCPROP_AUTHOR                    = WideString('author');}
  SZ_THDOCPROP_DISPLAYNAME: PWideChar    = 'DisplayName';
  SZ_THDOCPROP_CANONICALNAME: PWideChar  = 'ThemeName';
  SZ_THDOCPROP_TOOLTIP: PWideChar        = 'ToolTip';
  SZ_THDOCPROP_AUTHOR: PWideChar         = 'author';

var
  GetThemeDocumentationProperty: function(pszThemeName, pszPropertyName: LPCWSTR;
    pszValueBuff: LPWSTR; cchMaxValChars: Integer): HRESULT; stdcall;
  DrawThemeParentBackground: function(hwnd: HWND; hdc: HDC; prc: PRECT): HRESULT; stdcall;
  EnableTheming: function(fEnable: BOOL): HRESULT; stdcall;

{ Tmschema.h }

type
  BGTYPE = Integer;
const
  BT_IMAGEFILE = 0;
  BT_BORDERFILL = 1;
  BT_NONE = 2;

type
  IMAGELAYOUT = Integer;
const
  IL_VERTICAL = 0;
  IL_HORIZONTAL = 1;

type
  BORDERTYPE = Integer;
const
  BT_RECT = 0;
  BT_ROUNDRECT = 1;
  BT_ELLIPSE = 2;

type
  FILLTYPE = Integer;
const
  FT_SOLID = 0;
  FT_VERTGRADIENT = 1;
  FT_HORZGRADIENT = 2;
  FT_RADIALGRADIENT = 3;
  FT_TILEIMAGE = 4;

type
  SIZINGTYPE = Integer;
const
  ST_TRUESIZE = 0;
  ST_STRETCH = 1;
  ST_TILE = 2;

type
  HALIGN = Integer;
const
  HA_LEFT = 0;
  HA_CENTER = 1;
  HA_RIGHT = 2;

type
  CONTENTALIGNMENT = Integer;
const
  CA_LEFT = 0;
  CA_CENTER = 1;
  CA_RIGHT = 2;

type
  VALIGN = Integer;
const
  VA_TOP = 0;
  VA_CENTER = 1;
  VA_BOTTOM = 2;

type
  OFFSETTYPE = Integer;
const
  OT_TOPLEFT = 0;
  OT_TOPRIGHT = 1;
  OT_TOPMIDDLE = 2;
  OT_BOTTOMLEFT = 3;
  OT_BOTTOMRIGHT = 4;
  OT_BOTTOMMIDDLE = 5;
  OT_MIDDLELEFT = 6;
  OT_MIDDLERIGHT = 7;
  OT_LEFTOFCAPTION = 8;
  OT_RIGHTOFCAPTION = 9;
  OT_LEFTOFLASTBUTTON = 10;
  OT_RIGHTOFLASTBUTTON = 11;
  OT_ABOVELASTBUTTON = 12;
  OT_BELOWLASTBUTTON = 13;

type
  ICONEFFECT = Integer;
const
  ICE_NONE = 0;
  ICE_GLOW = 1;
  ICE_SHADOW = 2;
  ICE_PULSE = 3;
  ICE_ALPHA = 4;

type
  TEXTSHADOWTYPE = Integer;
const
  TST_NONE = 0;
  TST_SINGLE = 1;
  TST_CONTINUOUS = 2;

type
  GLYPHTYPE = Integer;
const
  GT_NONE = 0;
  GT_IMAGEGLYPH = 1;
  GT_FONTGLYPH = 2;

type
  IMAGESELECTTYPE = Integer;
const
  IST_NONE = 0;
  IST_SIZE = 1;
  IST_DPI = 2;

type
  TRUESIZESCALINGTYPE = Integer;
const
  TSST_NONE = 0;
  TSST_SIZE = 1;
  TSST_DPI = 2;

type
  GLYPHFONTSIZINGTYPE = Integer;
const
  GFST_NONE = 0;
  GFST_SIZE = 1;
  GFST_DPI = 2;

type
  PropValues = Integer;
const
  DummyProp = 49;

  TMT_RESERVEDLOW = 0; 
  TMT_RESERVEDHIGH        = 7999; 

  TMT_DIBDATA     = 2; 
  TMT_GLYPHDIBDATA        = 8; 
  TMT_ENUM        = 200; 
  TMT_STRING      = 201; 
  TMT_INT = 202; 
  TMT_BOOL        = 203; 
  TMT_COLOR       = 204; 
  TMT_MARGINS     = 205; 
  TMT_FILENAME    = 206; 
  TMT_SIZE        = 207; 
  TMT_POSITION    = 208; 
  TMT_RECT        = 209; 
  TMT_FONT        = 210; 
  TMT_INTLIST     = 211; 
  TMT_HBITMAP     = 212; 
  TMT_DISKSTREAM  = 213; 
  TMT_STREAM      = 214; 
  TMT_BITMAPREF   = 215; 
  TMT_COLORSCHEMES        = 401; 
  TMT_SIZES       = 402; 
  TMT_CHARSET     = 403; 
  TMT_NAME        = 600; 
  TMT_DISPLAYNAME = 601; 
  TMT_TOOLTIP     = 602; 
  TMT_COMPANY     = 603; 
  TMT_AUTHOR      = 604; 
  TMT_COPYRIGHT   = 605; 
  TMT_URL = 606; 
  TMT_VERSION     = 607; 
  TMT_DESCRIPTION = 608; 
  TMT_FIRST_RCSTRING_NAME = TMT_DISPLAYNAME; 
  TMT_LAST_RCSTRING_NAME  = TMT_DESCRIPTION; 
  TMT_CAPTIONFONT = 801; 
  TMT_SMALLCAPTIONFONT    = 802; 
  TMT_MENUFONT    = 803; 
  TMT_STATUSFONT  = 804; 
  TMT_MSGBOXFONT  = 805; 
  TMT_ICONTITLEFONT       = 806; 
  TMT_HEADING1FONT        = 807; 
  TMT_HEADING2FONT        = 808; 
  TMT_BODYFONT    = 809; 
  TMT_FIRSTFONT   = TMT_CAPTIONFONT; 
  TMT_LASTFONT    = TMT_BODYFONT; 
  TMT_FLATMENUS   = 1001; 
  TMT_FIRSTBOOL   = TMT_FLATMENUS; 
  TMT_LASTBOOL    = TMT_FLATMENUS; 
  TMT_SIZINGBORDERWIDTH   = 1201; 
  TMT_SCROLLBARWIDTH      = 1202; 
  TMT_SCROLLBARHEIGHT     = 1203; 
  TMT_CAPTIONBARWIDTH     = 1204; 
  TMT_CAPTIONBARHEIGHT    = 1205; 
  TMT_SMCAPTIONBARWIDTH   = 1206; 
  TMT_SMCAPTIONBARHEIGHT  = 1207; 
  TMT_MENUBARWIDTH        = 1208; 
  TMT_MENUBARHEIGHT       = 1209; 
  TMT_PADDEDBORDERWIDTH   = 1210; 
  TMT_FIRSTSIZE   = TMT_SIZINGBORDERWIDTH; 
  TMT_LASTSIZE    = TMT_PADDEDBORDERWIDTH; 
  TMT_MINCOLORDEPTH       = 1301; 
  TMT_FIRSTINT    = TMT_MINCOLORDEPTH; 
  TMT_LASTINT     = TMT_MINCOLORDEPTH; 
  TMT_CSSNAME     = 1401; 
  TMT_XMLNAME     = 1402; 
  TMT_LASTUPDATED = 1403; 
  TMT_ALIAS       = 1404; 
  TMT_FIRSTSTRING = TMT_CSSNAME; 
  TMT_LASTSTRING  = TMT_ALIAS; 
  TMT_SCROLLBAR   = 1601; 
  TMT_BACKGROUND  = 1602; 
  TMT_ACTIVECAPTION       = 1603; 
  TMT_INACTIVECAPTION     = 1604; 
  TMT_MENU        = 1605; 
  TMT_WINDOW      = 1606; 
  TMT_WINDOWFRAME = 1607; 
  TMT_MENUTEXT    = 1608; 
  TMT_WINDOWTEXT  = 1609; 
  TMT_CAPTIONTEXT = 1610; 
  TMT_ACTIVEBORDER        = 1611; 
  TMT_INACTIVEBORDER      = 1612; 
  TMT_APPWORKSPACE        = 1613; 
  TMT_HIGHLIGHT   = 1614; 
  TMT_HIGHLIGHTTEXT       = 1615; 
  TMT_BTNFACE     = 1616; 
  TMT_BTNSHADOW   = 1617; 
  TMT_GRAYTEXT    = 1618; 
  TMT_BTNTEXT     = 1619; 
  TMT_INACTIVECAPTIONTEXT = 1620; 
  TMT_BTNHIGHLIGHT        = 1621; 
  TMT_DKSHADOW3D  = 1622; 
  TMT_LIGHT3D     = 1623; 
  TMT_INFOTEXT    = 1624; 
  TMT_INFOBK      = 1625; 
  TMT_BUTTONALTERNATEFACE = 1626; 
  TMT_HOTTRACKING = 1627; 
  TMT_GRADIENTACTIVECAPTION       = 1628; 
  TMT_GRADIENTINACTIVECAPTION     = 1629; 
  TMT_MENUHILIGHT = 1630; 
  TMT_MENUBAR     = 1631; 
  TMT_FIRSTCOLOR  = TMT_SCROLLBAR; 
  TMT_LASTCOLOR   = TMT_MENUBAR; 
  TMT_FROMHUE1    = 1801; 
  TMT_FROMHUE2    = 1802; 
  TMT_FROMHUE3    = 1803; 
  TMT_FROMHUE4    = 1804; 
  TMT_FROMHUE5    = 1805; 
  TMT_TOHUE1      = 1806; 
  TMT_TOHUE2      = 1807; 
  TMT_TOHUE3      = 1808; 
  TMT_TOHUE4      = 1809; 
  TMT_TOHUE5      = 1810; 
  TMT_FROMCOLOR1  = 2001; 
  TMT_FROMCOLOR2  = 2002; 
  TMT_FROMCOLOR3  = 2003; 
  TMT_FROMCOLOR4  = 2004; 
  TMT_FROMCOLOR5  = 2005; 
  TMT_TOCOLOR1    = 2006; 
  TMT_TOCOLOR2    = 2007; 
  TMT_TOCOLOR3    = 2008; 
  TMT_TOCOLOR4    = 2009; 
  TMT_TOCOLOR5    = 2010; 
  TMT_TRANSPARENT = 2201; 
  TMT_AUTOSIZE    = 2202; 
  TMT_BORDERONLY  = 2203; 
  TMT_COMPOSITED  = 2204; 
  TMT_BGFILL      = 2205; 
  TMT_GLYPHTRANSPARENT    = 2206; 
  TMT_GLYPHONLY   = 2207; 
  TMT_ALWAYSSHOWSIZINGBAR = 2208; 
  TMT_MIRRORIMAGE = 2209; 
  TMT_UNIFORMSIZING       = 2210; 
  TMT_INTEGRALSIZING      = 2211; 
  TMT_SOURCEGROW  = 2212; 
  TMT_SOURCESHRINK        = 2213; 
  TMT_DRAWBORDERS = 2214; 
  TMT_NOETCHEDEFFECT      = 2215; 
  TMT_TEXTAPPLYOVERLAY    = 2216; 
  TMT_TEXTGLOW    = 2217; 
  TMT_TEXTITALIC  = 2218; 
  TMT_COMPOSITEDOPAQUE    = 2219; 
  TMT_LOCALIZEDMIRRORIMAGE        = 2220; 
  TMT_IMAGECOUNT  = 2401; 
  TMT_ALPHALEVEL  = 2402; 
  TMT_BORDERSIZE  = 2403; 
  TMT_ROUNDCORNERWIDTH    = 2404; 
  TMT_ROUNDCORNERHEIGHT   = 2405; 
  TMT_GRADIENTRATIO1      = 2406; 
  TMT_GRADIENTRATIO2      = 2407; 
  TMT_GRADIENTRATIO3      = 2408; 
  TMT_GRADIENTRATIO4      = 2409; 
  TMT_GRADIENTRATIO5      = 2410; 
  TMT_PROGRESSCHUNKSIZE   = 2411; 
  TMT_PROGRESSSPACESIZE   = 2412; 
  TMT_SATURATION  = 2413; 
  TMT_TEXTBORDERSIZE      = 2414; 
  TMT_ALPHATHRESHOLD      = 2415; 
  TMT_WIDTH       = 2416; 
  TMT_HEIGHT      = 2417; 
  TMT_GLYPHINDEX  = 2418; 
  TMT_TRUESIZESTRETCHMARK = 2419; 
  TMT_MINDPI1     = 2420; 
  TMT_MINDPI2     = 2421; 
  TMT_MINDPI3     = 2422; 
  TMT_MINDPI4     = 2423; 
  TMT_MINDPI5     = 2424; 
  TMT_TEXTGLOWSIZE        = 2425; 
  TMT_FRAMESPERSECOND     = 2426; 
  TMT_PIXELSPERFRAME      = 2427; 
  TMT_ANIMATIONDELAY      = 2428; 
  TMT_GLOWINTENSITY       = 2429; 
  TMT_OPACITY     = 2430; 
  TMT_COLORIZATIONCOLOR   = 2431; 
  TMT_COLORIZATIONOPACITY = 2432; 
  TMT_GLYPHFONT   = 2601; 
  TMT_IMAGEFILE   = 3001; 
  TMT_IMAGEFILE1  = 3002; 
  TMT_IMAGEFILE2  = 3003; 
  TMT_IMAGEFILE3  = 3004; 
  TMT_IMAGEFILE4  = 3005; 
  TMT_IMAGEFILE5  = 3006; 
  TMT_GLYPHIMAGEFILE      = 3008; 
  TMT_TEXT        = 3201; 
  TMT_CLASSICVALUE        = 3202; 
  TMT_OFFSET      = 3401; 
  TMT_TEXTSHADOWOFFSET    = 3402; 
  TMT_MINSIZE     = 3403; 
  TMT_MINSIZE1    = 3404; 
  TMT_MINSIZE2    = 3405; 
  TMT_MINSIZE3    = 3406; 
  TMT_MINSIZE4    = 3407; 
  TMT_MINSIZE5    = 3408; 
  TMT_NORMALSIZE  = 3409; 
  TMT_SIZINGMARGINS       = 3601; 
  TMT_CONTENTMARGINS      = 3602; 
  TMT_CAPTIONMARGINS      = 3603; 
  TMT_BORDERCOLOR = 3801; 
  TMT_FILLCOLOR   = 3802; 
  TMT_TEXTCOLOR   = 3803; 
  TMT_EDGELIGHTCOLOR      = 3804; 
  TMT_EDGEHIGHLIGHTCOLOR  = 3805; 
  TMT_EDGESHADOWCOLOR     = 3806; 
  TMT_EDGEDKSHADOWCOLOR   = 3807; 
  TMT_EDGEFILLCOLOR       = 3808; 
  TMT_TRANSPARENTCOLOR    = 3809; 
  TMT_GRADIENTCOLOR1      = 3810; 
  TMT_GRADIENTCOLOR2      = 3811; 
  TMT_GRADIENTCOLOR3      = 3812; 
  TMT_GRADIENTCOLOR4      = 3813; 
  TMT_GRADIENTCOLOR5      = 3814; 
  TMT_SHADOWCOLOR = 3815; 
  TMT_GLOWCOLOR   = 3816; 
  TMT_TEXTBORDERCOLOR     = 3817; 
  TMT_TEXTSHADOWCOLOR     = 3818; 
  TMT_GLYPHTEXTCOLOR      = 3819; 
  TMT_GLYPHTRANSPARENTCOLOR       = 3820; 
  TMT_FILLCOLORHINT       = 3821; 
  TMT_BORDERCOLORHINT     = 3822; 
  TMT_ACCENTCOLORHINT     = 3823; 
  TMT_TEXTCOLORHINT       = 3824; 
  TMT_HEADING1TEXTCOLOR   = 3825; 
  TMT_HEADING2TEXTCOLOR   = 3826; 
  TMT_BODYTEXTCOLOR       = 3827; 
  TMT_BGTYPE      = 4001; 
  TMT_BORDERTYPE  = 4002; 
  TMT_FILLTYPE    = 4003; 
  TMT_SIZINGTYPE  = 4004; 
  TMT_HALIGN      = 4005; 
  TMT_CONTENTALIGNMENT    = 4006; 
  TMT_VALIGN      = 4007; 
  TMT_OFFSETTYPE  = 4008; 
  TMT_ICONEFFECT  = 4009; 
  TMT_TEXTSHADOWTYPE      = 4010; 
  TMT_IMAGELAYOUT = 4011; 
  TMT_GLYPHTYPE   = 4012; 
  TMT_IMAGESELECTTYPE     = 4013; 
  TMT_GLYPHFONTSIZINGTYPE = 4014; 
  TMT_TRUESIZESCALINGTYPE = 4015; 
  TMT_USERPICTURE = 5001; 
  TMT_DEFAULTPANESIZE     = 5002; 
  TMT_BLENDCOLOR  = 5003; 
  TMT_CUSTOMSPLITRECT     = 5004; 
  TMT_ANIMATIONBUTTONRECT = 5005; 
  TMT_ANIMATIONDURATION   = 5006; 
  TMT_TRANSITIONDURATIONS = 6000; 
  TMT_SCALEDBACKGROUND    = 7001; 
  TMT_ATLASIMAGE  = 8000; 
  TMT_ATLASINPUTIMAGE     = 8001; 
  TMT_ATLASRECT   = 8002; 

const
  { For Windows >= Vista }
  TMTVS_RESERVEDLOW       = 100000;
  TMTVS_RESERVEDHIGH      = 19999;


// 
//  WINDOWSTYLE class parts and states
// 
const
  VSCLASS_WINDOWSTYLE     = 'WINDOWSTYLE';
  VSCLASS_WINDOW  = 'WINDOW';

type
  WINDOWPARTS = Integer;
const
  WINDOWPartFiller0 = 0;
  WP_CAPTION = 1;
  WP_SMALLCAPTION = 2;
  WP_MINCAPTION = 3;
  WP_SMALLMINCAPTION = 4;
  WP_MAXCAPTION = 5;
  WP_SMALLMAXCAPTION = 6;
  WP_FRAMELEFT = 7;
  WP_FRAMERIGHT = 8;
  WP_FRAMEBOTTOM = 9;
  WP_SMALLFRAMELEFT = 10;
  WP_SMALLFRAMERIGHT = 11;
  WP_SMALLFRAMEBOTTOM = 12;
  WP_SYSBUTTON = 13;
  WP_MDISYSBUTTON = 14;
  WP_MINBUTTON = 15;
  WP_MDIMINBUTTON = 16;
  WP_MAXBUTTON = 17;
  WP_CLOSEBUTTON = 18;
  WP_SMALLCLOSEBUTTON = 19;
  WP_MDICLOSEBUTTON = 20;
  WP_RESTOREBUTTON = 21;
  WP_MDIRESTOREBUTTON = 22;
  WP_HELPBUTTON = 23;
  WP_MDIHELPBUTTON = 24;
  WP_HORZSCROLL = 25;
  WP_HORZTHUMB = 26;
  WP_VERTSCROLL = 27;
  WP_VERTTHUMB = 28;
  WP_DIALOG = 29;
  WP_CAPTIONSIZINGTEMPLATE = 30;
  WP_SMALLCAPTIONSIZINGTEMPLATE = 31;
  WP_FRAMELEFTSIZINGTEMPLATE = 32;
  WP_SMALLFRAMELEFTSIZINGTEMPLATE = 33;
  WP_FRAMERIGHTSIZINGTEMPLATE = 34;
  WP_SMALLFRAMERIGHTSIZINGTEMPLATE = 35;
  WP_FRAMEBOTTOMSIZINGTEMPLATE = 36;
  WP_SMALLFRAMEBOTTOMSIZINGTEMPLATE = 37;
  { For Windows >= Vista }
  WP_FRAME     = 38;

type
  FRAMESTATES = Integer;
const
  FRAMEStateFiller0 = 0;
  FS_ACTIVE = 1;
  FS_INACTIVE = 2;

type
  CAPTIONSTATES = Integer;
const
  CAPTIONStateFiller0 = 0;
  CS_ACTIVE = 1;
  CS_INACTIVE = 2;
  CS_DISABLED = 3;

type
  MAXCAPTIONSTATES = Integer;
const
  MAXCAPTIONStateFiller0 = 0;
  MXCS_ACTIVE = 1;
  MXCS_INACTIVE = 2;
  MXCS_DISABLED = 3;

type
  MINCAPTIONSTATES = Integer;
const
  MINCAPTIONStateFiller0 = 0;
  MNCS_ACTIVE = 1;
  MNCS_INACTIVE = 2;
  MNCS_DISABLED = 3;

type
  HORZSCROLLSTATES = Integer;
const
  HORZSCROLLStateFiller0 = 0;
  HSS_NORMAL = 1;
  HSS_HOT = 2;
  HSS_PUSHED = 3;
  HSS_DISABLED = 4;

type
  HORZTHUMBSTATES = Integer;
const
  HORZTHUMBStateFiller0 = 0;
  HTS_NORMAL = 1;
  HTS_HOT = 2;
  HTS_PUSHED = 3;
  HTS_DISABLED = 4;

type
  VERTSCROLLSTATES = Integer;
const
  VERTSCROLLStateFiller0 = 0;
  VSS_NORMAL = 1;
  VSS_HOT = 2;
  VSS_PUSHED = 3;
  VSS_DISABLED = 4;

type
  VERTTHUMBSTATES = Integer;
const
  VERTTHUMBStateFiller0 = 0;
  VTS_NORMAL = 1;
  VTS_HOT = 2;
  VTS_PUSHED = 3;
  VTS_DISABLED = 4;

type
  SYSBUTTONSTATES = Integer;
const
  SYSBUTTONStateFiller0 = 0;
  SBS_NORMAL = 1;
  SBS_HOT = 2;
  SBS_PUSHED = 3;
  SBS_DISABLED = 4;

type
  MINBUTTONSTATES = Integer;
const
  MINBUTTONStateFiller0 = 0;
  MINBS_NORMAL = 1;
  MINBS_HOT = 2;
  MINBS_PUSHED = 3;
  MINBS_DISABLED = 4;

type
  MAXBUTTONSTATES = Integer;
const
  MAXBUTTONStateFiller0 = 0;
  MAXBS_NORMAL = 1;
  MAXBS_HOT = 2;
  MAXBS_PUSHED = 3;
  MAXBS_DISABLED = 4;

type
  RESTOREBUTTONSTATES = Integer;
const
  RESTOREBUTTONStateFiller0 = 0;
  RBS_NORMAL = 1;
  RBS_HOT = 2;
  RBS_PUSHED = 3;
  RBS_DISABLED = 4;

type
  HELPBUTTONSTATES = Integer;
const
  HELPBUTTONStateFiller0 = 0;
  HBS_NORMAL = 1;
  HBS_HOT = 2;
  HBS_PUSHED = 3;
  HBS_DISABLED = 4;

type
  CLOSEBUTTONSTATES = Integer;
const
  CLOSEBUTTONStateFiller0 = 0;
  CBS_NORMAL = 1;
  CBS_HOT = 2;
  CBS_PUSHED = 3;
  CBS_DISABLED = 4;

  
//
//  BUTTONSTYLE class parts and states
//
const
  VSCLASS_BUTTONSTYLE     = 'BUTTONSTYLE';
  VSCLASS_BUTTON  = 'BUTTON';

type
  BUTTONPARTS = Integer;
const
  BUTTONPartFiller0 = 0;
  BP_PUSHBUTTON = 1;
  BP_RADIOBUTTON = 2;
  BP_CHECKBOX = 3;
  BP_GROUPBOX = 4;
  BP_USERBUTTON = 5;
  { For Windows >= Vista }
  BP_COMMANDLINK = 6;
  BP_COMMANDLINKGLYPH = 7;

type
  PUSHBUTTONSTATES = Integer;
const
  PUSHBUTTONStateFiller0 = 0;
  PBS_NORMAL = 1;
  PBS_HOT = 2;
  PBS_PRESSED = 3;
  PBS_DISABLED = 4;
  PBS_DEFAULTED = 5;
  { For Windows >= Vista }
  PBS_DEFAULTED_ANIMATING     = 6;

type
  RADIOBUTTONSTATES = Integer;
const
  RADIOBUTTONStateFiller0 = 0;
  RBS_UNCHECKEDNORMAL = 1;
  RBS_UNCHECKEDHOT = 2;
  RBS_UNCHECKEDPRESSED = 3;
  RBS_UNCHECKEDDISABLED = 4;
  RBS_CHECKEDNORMAL = 5;
  RBS_CHECKEDHOT = 6;
  RBS_CHECKEDPRESSED = 7;
  RBS_CHECKEDDISABLED = 8;

type
  CHECKBOXSTATES = Integer;
const
  CHECKBOXStateFiller0 = 0;
  CBS_UNCHECKEDNORMAL = 1;
  CBS_UNCHECKEDHOT = 2;
  CBS_UNCHECKEDPRESSED = 3;
  CBS_UNCHECKEDDISABLED = 4;
  CBS_CHECKEDNORMAL = 5;
  CBS_CHECKEDHOT = 6;
  CBS_CHECKEDPRESSED = 7;
  CBS_CHECKEDDISABLED = 8;
  CBS_MIXEDNORMAL = 9;
  CBS_MIXEDHOT = 10;
  CBS_MIXEDPRESSED = 11;
  CBS_MIXEDDISABLED = 12;
  { For Windows >= Vista }
  CBS_IMPLICITNORMAL     = 13;
  CBS_IMPLICITHOT     = 14;
  CBS_IMPLICITPRESSED     = 15;
  CBS_IMPLICITDISABLED     = 16;
  CBS_EXCLUDEDNORMAL     = 17;
  CBS_EXCLUDEDHOT     = 18;
  CBS_EXCLUDEDPRESSED     = 19;
  CBS_EXCLUDEDDISABLED     = 20;

type
  GROUPBOXSTATES = Integer;
const
  GROUPBOXStateFiller0 = 0;
  GBS_NORMAL = 1;
  GBS_DISABLED = 2;

type
  { For Windows >= Vista }
  COMMANDLINKSTATES = Integer;
const
  CMDLS_NORMAL     = 1;
  CMDLS_HOT     = 2;
  CMDLS_PRESSED     = 3;
  CMDLS_DISABLED     = 4;
  CMDLS_DEFAULTED     = 5;
  CMDLS_DEFAULTED_ANIMATING     = 6;

type
  { For Windows >= Vista }
  COMMANDLINKGLYPHSTATES = Integer;
const
  CMDLGS_NORMAL     = 1;
  CMDLGS_HOT     = 2;
  CMDLGS_PRESSED     = 3;
  CMDLGS_DISABLED     = 4;
  CMDLGS_DEFAULTED     = 5;


//
//  REBARSTYLE class parts and states
//
const
  VSCLASS_REBARSTYLE      = 'REBARSTYLE';
  VSCLASS_REBAR   = 'REBAR';

type
  REBARPARTS = Integer;
const
  REBARPartFiller0 = 0;
  RP_GRIPPER = 1;
  RP_GRIPPERVERT = 2;
  RP_BAND = 3;
  RP_CHEVRON = 4;
  RP_CHEVRONVERT = 5;
  { For Windows >= Vista }
  RP_BACKGROUND     = 6;
  RP_SPLITTER     = 7;
  RP_SPLITTERVERT     = 8;

type
  CHEVRONSTATES = Integer;
const
  CHEVRONStateFiller0 = 0;
  CHEVS_NORMAL = 1;
  CHEVS_HOT = 2;
  CHEVS_PRESSED = 3;

type
  { For Windows >= Vista }
  CHEVRONVERTSTATES = Integer;
const
  CHEVSV_NORMAL     = 1;
  CHEVSV_HOT     = 2;
  CHEVSV_PRESSED     = 3;

type
  { For Windows >= Vista }
  SPLITTERSTATES = Integer;
const
  SPLITS_NORMAL     = 1;
  SPLITS_HOT     = 2;
  SPLITS_PRESSED     = 3;

type
  { For Windows >= Vista }
  SPLITTERVERTSTATES = Integer;
const
  SPLITSV_NORMAL     = 1;
  SPLITSV_HOT     = 2;
  SPLITSV_PRESSED     = 3;

  
//
//  TOOLBARSTYLE class parts and states
// 
const
  VSCLASS_TOOLBARSTYLE    = 'TOOLBARSTYLE';
  VSCLASS_TOOLBAR = 'TOOLBAR';

type
  TOOLBARPARTS = Integer;
const
  TOOLBARPartFiller0 = 0;
  TP_BUTTON = 1;
  TP_DROPDOWNBUTTON = 2;
  TP_SPLITBUTTON = 3;
  TP_SPLITBUTTONDROPDOWN = 4;
  TP_SEPARATOR = 5;
  TP_SEPARATORVERT = 6;

type
  TOOLBARSTATES = Integer;
const
  TOOLBARStateFiller0 = 0;
  TS_NORMAL = 1;
  TS_HOT = 2;
  TS_PRESSED = 3;
  TS_DISABLED = 4;
  TS_CHECKED = 5;
  TS_HOTCHECKED = 6;
  { For Windows >= Vista }
  TS_NEARHOT     = 7;
  TS_OTHERSIDEHOT     = 8;

  
//
//  STATUSSTYLE class parts and states
// 
const
  VSCLASS_STATUSSTYLE     = 'STATUSSTYLE';
  VSCLASS_STATUS  = 'STATUS';

type
  STATUSPARTS = Integer;
const
  STATUSPartFiller0 = 0;
  SP_PANE = 1;
  SP_GRIPPERPANE = 2;
  SP_GRIPPER = 3;

  
//
//  MENUSTYLE class parts and states
//
const
  VSCLASS_MENUSTYLE       = 'MENUSTYLE';
  VSCLASS_MENU    = 'MENU';

type
  MENUPARTS = Integer;
const
  MENUPartFiller0 = 0;
  MP_MENUITEM = 1;
  MP_MENUDROPDOWN = 2;
  MP_MENUBARITEM = 3;
  MP_MENUBARDROPDOWN = 4;
  MP_CHEVRON = 5;
  MP_SEPARATOR = 6;

  { For Windows >= Vista }
  MENU_MENUITEM_TMSCHEMA     = 1; 
  MENU_MENUDROPDOWN_TMSCHEMA     = 2; 
  MENU_MENUBARITEM_TMSCHEMA     = 3; 
  MENU_MENUBARDROPDOWN_TMSCHEMA     = 4; 
  MENU_CHEVRON_TMSCHEMA     = 5; 
  MENU_SEPARATOR_TMSCHEMA     = 6; 
  MENU_BARBACKGROUND     = 7; 
  MENU_BARITEM     = 8; 
  MENU_POPUPBACKGROUND     = 9; 
  MENU_POPUPBORDERS     = 10; 
  MENU_POPUPCHECK     = 11; 
  MENU_POPUPCHECKBACKGROUND     = 12; 
  MENU_POPUPGUTTER     = 13; 
  MENU_POPUPITEM     = 14; 
  MENU_POPUPSEPARATOR     = 15; 
  MENU_POPUPSUBMENU     = 16; 
  MENU_SYSTEMCLOSE     = 17; 
  MENU_SYSTEMMAXIMIZE     = 18; 
  MENU_SYSTEMMINIMIZE     = 19; 
  MENU_SYSTEMRESTORE     = 20; 

type
  MENUSTATES = Integer;
const
  MENUStateFiller0 = 0;
  MS_NORMAL = 1;
  MS_SELECTED = 2;
  MS_DEMOTED = 3;

type
  { For Windows >= Vista }
  BARBACKGROUNDSTATES = Integer;
const
  MB_ACTIVE     = 1;
  MB_INACTIVE     = 2;

type
  { For Windows >= Vista }
  BARITEMSTATES = Integer;
const
  MBI_NORMAL     = 1;
  MBI_HOT     = 2;
  MBI_PUSHED     = 3;
  MBI_DISABLED     = 4;
  MBI_DISABLEDHOT     = 5;
  MBI_DISABLEDPUSHED     = 6;

type
  { For Windows >= Vista }
  POPUPCHECKSTATES = Integer;
const
  MC_CHECKMARKNORMAL     = 1;
  MC_CHECKMARKDISABLED     = 2;
  MC_BULLETNORMAL     = 3;
  MC_BULLETDISABLED     = 4;

type
  { For Windows >= Vista }
  POPUPCHECKBACKGROUNDSTATES = Integer;
const
  MCB_DISABLED     = 1;
  MCB_NORMAL     = 2;
  MCB_BITMAP     = 3;

type
  { For Windows >= Vista }
  POPUPITEMSTATES = Integer;
const
  MPI_NORMAL     = 1;
  MPI_HOT     = 2;
  MPI_DISABLED     = 3;
  MPI_DISABLEDHOT     = 4;

type
  { For Windows >= Vista }
  POPUPSUBMENUSTATES = Integer;
const
  MSM_NORMAL     = 1;
  MSM_DISABLED     = 2;

type
  { For Windows >= Vista }
  SYSTEMCLOSESTATES = Integer;
const
  MSYSC_NORMAL     = 1;
  MSYSC_DISABLED     = 2;

type
  { For Windows >= Vista }
  SYSTEMMAXIMIZESTATES = Integer;
const
  MSYSMX_NORMAL     = 1;
  MSYSMX_DISABLED     = 2;

type
  { For Windows >= Vista }
  SYSTEMMINIMIZESTATES = Integer;
const
  MSYSMN_NORMAL     = 1;
  MSYSMN_DISABLED     = 2;

type
  { For Windows >= Vista }
  SYSTEMRESTORESTATES = Integer;
const
  MSYSR_NORMAL     = 1;
  MSYSR_DISABLED     = 2;
  

//
//  LISTVIEWSTYLE class parts and states
//
const
  VSCLASS_LISTVIEWSTYLE   = 'LISTVIEWSTYLE';
  VSCLASS_LISTVIEW        = 'LISTVIEW';

type
  LISTVIEWPARTS = Integer;
const
  LISTVIEWPartFiller0 = 0;
  LVP_LISTITEM = 1;
  LVP_LISTGROUP = 2;
  LVP_LISTDETAIL = 3;
  LVP_LISTSORTEDDETAIL = 4;
  LVP_EMPTYTEXT = 5;
  { For Windows >= Vista }
  LVP_GROUPHEADER     = 6;
  LVP_GROUPHEADERLINE     = 7;
  LVP_EXPANDBUTTON     = 8;
  LVP_COLLAPSEBUTTON     = 9;
  LVP_COLUMNDETAIL     = 10;

type
  LISTITEMSTATES = Integer;
const
  LISTITEMStateFiller0 = 0;
  LIS_NORMAL = 1;
  LIS_HOT = 2;
  LIS_SELECTED = 3;
  LIS_DISABLED = 4;
  LIS_SELECTEDNOTFOCUS = 5;
  { For Windows >= Vista }
  LISS_HOTSELECTED     = 6;

type
  { For Windows >= Vista }
  GROUPHEADERSTATES = Integer;
const
  LVGH_OPEN     = 1;
  LVGH_OPENHOT     = 2;
  LVGH_OPENSELECTED     = 3;
  LVGH_OPENSELECTEDHOT     = 4;
  LVGH_OPENSELECTEDNOTFOCUSED     = 5;
  LVGH_OPENSELECTEDNOTFOCUSEDHOT     = 6;
  LVGH_OPENMIXEDSELECTION     = 7;
  LVGH_OPENMIXEDSELECTIONHOT     = 8;
  LVGH_CLOSE     = 9;
  LVGH_CLOSEHOT     = 10;
  LVGH_CLOSESELECTED     = 11;
  LVGH_CLOSESELECTEDHOT     = 12;
  LVGH_CLOSESELECTEDNOTFOCUSED     = 13;
  LVGH_CLOSESELECTEDNOTFOCUSEDHOT     = 14;
  LVGH_CLOSEMIXEDSELECTION     = 15;
  LVGH_CLOSEMIXEDSELECTIONHOT     = 16;

type
  { For Windows >= Vista }
  GROUPHEADERLINESTATES = Integer;
const
  LVGHL_OPEN     = 1;
  LVGHL_OPENHOT     = 2;
  LVGHL_OPENSELECTED     = 3;
  LVGHL_OPENSELECTEDHOT     = 4;
  LVGHL_OPENSELECTEDNOTFOCUSED     = 5;
  LVGHL_OPENSELECTEDNOTFOCUSEDHOT     = 6;
  LVGHL_OPENMIXEDSELECTION     = 7;
  LVGHL_OPENMIXEDSELECTIONHOT     = 8;
  LVGHL_CLOSE     = 9;
  LVGHL_CLOSEHOT     = 10;
  LVGHL_CLOSESELECTED     = 11;
  LVGHL_CLOSESELECTEDHOT     = 12;
  LVGHL_CLOSESELECTEDNOTFOCUSED     = 13;
  LVGHL_CLOSESELECTEDNOTFOCUSEDHOT     = 14;
  LVGHL_CLOSEMIXEDSELECTION     = 15;
  LVGHL_CLOSEMIXEDSELECTIONHOT     = 16;

type
  { For Windows >= Vista }
  EXPANDBUTTONSTATES = Integer;
const
  LVEB_NORMAL     = 1;
  LVEB_HOVER     = 2;
  LVEB_PUSHED     = 3;

type
  { For Windows >= Vista }
  COLLAPSEBUTTONSTATES = Integer;
const
  LVCB_NORMAL     = 1;
  LVCB_HOVER     = 2;
  LVCB_PUSHED     = 3;

  
//
//  HEADERSTYLE class parts and states
//
const
  VSCLASS_HEADERSTYLE     = 'HEADERSTYLE';
  VSCLASS_HEADER  = 'HEADER';

type
  HEADERPARTS = Integer;
const
  HEADERPartFiller0 = 0;
  HP_HEADERITEM = 1;
  HP_HEADERITEMLEFT = 2;
  HP_HEADERITEMRIGHT = 3;
  HP_HEADERSORTARROW = 4;
  { For Windows >= Vista }
  HP_HEADERDROPDOWN     = 5;
  HP_HEADERDROPDOWNFILTER     = 6;
  HP_HEADEROVERFLOW     = 7;

type
  { For Windows >= Vista }
  HEADERSTYLESTATES = Integer;
const
  HBG_DETAILS     = 1;
  HBG_ICON     = 2;

type
  HEADERITEMSTATES = Integer;
const
  HEADERITEMStateFiller0 = 0;
  HIS_NORMAL = 1;
  HIS_HOT = 2;
  HIS_PRESSED = 3;
  { For Windows >= Vista }
  HIS_SORTEDNORMAL     = 4;
  HIS_SORTEDHOT     = 5;
  HIS_SORTEDPRESSED     = 6;
  HIS_ICONNORMAL     = 7;
  HIS_ICONHOT     = 8;
  HIS_ICONPRESSED     = 9;
  HIS_ICONSORTEDNORMAL     = 10;
  HIS_ICONSORTEDHOT     = 11;
  HIS_ICONSORTEDPRESSED     = 12;

type
  HEADERITEMLEFTSTATES = Integer;
const
  HEADERITEMLEFTStateFiller0 = 0;
  HILS_NORMAL = 1;
  HILS_HOT = 2;
  HILS_PRESSED = 3;

type
  HEADERITEMRIGHTSTATES = Integer;
const
  HEADERITEMRIGHTStateFiller0 = 0;
  HIRS_NORMAL = 1;
  HIRS_HOT = 2;
  HIRS_PRESSED = 3;

type
  HEADERSORTARROWSTATES = Integer;
const
  HEADERSORTARROWStateFiller0 = 0;
  HSAS_SORTEDUP = 1;
  HSAS_SORTEDDOWN = 2;

type
  { For Windows >= Vista }
  HEADERDROPDOWNSTATES = Integer;
const
  HDDS_NORMAL     = 1;
  HDDS_SOFTHOT     = 2;
  HDDS_HOT     = 3;

type
  { For Windows >= Vista }
  HEADERDROPDOWNFILTERSTATES = Integer;
const
  HDDFS_NORMAL     = 1;
  HDDFS_SOFTHOT     = 2;
  HDDFS_HOT     = 3;

type
  { For Windows >= Vista }
  HEADEROVERFLOWSTATES = Integer;
const
  HOFS_NORMAL     = 1;
  HOFS_HOT     = 2;

  
//
//  PROGRESSSTYLE class parts and states
//
const
  VSCLASS_PROGRESSSTYLE   = 'PROGRESSSTYLE';
  VSCLASS_PROGRESS        = 'PROGRESS';

type
  PROGRESSPARTS = Integer;
const
  PROGRESSPartFiller0 = 0;
  PP_BAR = 1;
  PP_BARVERT = 2;
  PP_CHUNK = 3;
  PP_CHUNKVERT = 4;
  { For Windows >= Vista }
  PP_FILL     = 5;
  PP_FILLVERT     = 6;
  PP_PULSEOVERLAY     = 7;
  PP_MOVEOVERLAY     = 8;
  PP_PULSEOVERLAYVERT     = 9;
  PP_MOVEOVERLAYVERT     = 10;
  PP_TRANSPARENTBAR     = 11;
  PP_TRANSPARENTBARVERT     = 12;

type
  { For Windows >= Vista }
  TRANSPARENTBARSTATES = Integer;
const
  PBBS_NORMAL     = 1;
  PBBS_PARTIAL     = 2;

type
  { For Windows >= Vista }
  TRANSPARENTBARVERTSTATES = Integer;
const
  PBBVS_NORMAL     = 1;
  PBBVS_PARTIAL     = 2;

type
  { For Windows >= Vista }
  FILLSTATES = Integer;
const
  PBFS_NORMAL     = 1;
  PBFS_ERROR     = 2;
  PBFS_PAUSED     = 3;
  PBFS_PARTIAL     = 4;

type
  { For Windows >= Vista }
  FILLVERTSTATES = Integer;
const
  PBFVS_NORMAL     = 1;
  PBFVS_ERROR     = 2;
  PBFVS_PAUSED     = 3;
  PBFVS_PARTIAL     = 4;
  

//
//  TABSTYLE class parts and states
// 
const
  VSCLASS_TABSTYLE        = 'TABSTYLE';
  VSCLASS_TAB     = 'TAB';

type
  TABPARTS = Integer;
const
  TABPartFiller0 = 0;
  TABP_TABITEM = 1;
  TABP_TABITEMLEFTEDGE = 2;
  TABP_TABITEMRIGHTEDGE = 3;
  TABP_TABITEMBOTHEDGE = 4;
  TABP_TOPTABITEM = 5;
  TABP_TOPTABITEMLEFTEDGE = 6;
  TABP_TOPTABITEMRIGHTEDGE = 7;
  TABP_TOPTABITEMBOTHEDGE = 8;
  TABP_PANE = 9;
  TABP_BODY = 10;
  { For Windows >= Vista }
  TABP_AEROWIZARDBODY     = 11;

type
  TABITEMSTATES = Integer;
const
  TABITEMStateFiller0 = 0;
  TIS_NORMAL = 1;
  TIS_HOT = 2;
  TIS_SELECTED = 3;
  TIS_DISABLED = 4;
  TIS_FOCUSED = 5;

type
  TABITEMLEFTEDGESTATES = Integer;
const
  TABITEMLEFTEDGEStateFiller0 = 0;
  TILES_NORMAL = 1;
  TILES_HOT = 2;
  TILES_SELECTED = 3;
  TILES_DISABLED = 4;
  TILES_FOCUSED = 5;

type
  TABITEMRIGHTEDGESTATES = Integer;
const
  TABITEMRIGHTEDGEStateFiller0 = 0;
  TIRES_NORMAL = 1;
  TIRES_HOT = 2;
  TIRES_SELECTED = 3;
  TIRES_DISABLED = 4;
  TIRES_FOCUSED = 5;

type
  TABITEMBOTHEDGESSTATES = Integer;
const
  TABITEMBOTHEDGESStateFiller0 = 0;
  TIBES_NORMAL = 1;
  TIBES_HOT = 2;
  TIBES_SELECTED = 3;
  TIBES_DISABLED = 4;
  TIBES_FOCUSED = 5;

type
  TOPTABITEMSTATES = Integer;
const
  TOPTABITEMStateFiller0 = 0;
  TTIS_NORMAL = 1;
  TTIS_HOT = 2;
  TTIS_SELECTED = 3;
  TTIS_DISABLED = 4;
  TTIS_FOCUSED = 5;

type
  TOPTABITEMLEFTEDGESTATES = Integer;
const
  TOPTABITEMLEFTEDGEStateFiller0 = 0;
  TTILES_NORMAL = 1;
  TTILES_HOT = 2;
  TTILES_SELECTED = 3;
  TTILES_DISABLED = 4;
  TTILES_FOCUSED = 5;

type
  TOPTABITEMRIGHTEDGESTATES = Integer;
const
  TOPTABITEMRIGHTEDGEStateFiller0 = 0;
  TTIRES_NORMAL = 1;
  TTIRES_HOT = 2;
  TTIRES_SELECTED = 3;
  TTIRES_DISABLED = 4;
  TTIRES_FOCUSED = 5;

type
  TOPTABITEMBOTHEDGESSTATES = Integer;
const
  TOPTABITEMBOTHEDGESStateFiller0 = 0;
  TTIBES_NORMAL = 1;
  TTIBES_HOT = 2;
  TTIBES_SELECTED = 3;
  TTIBES_DISABLED = 4;
  TTIBES_FOCUSED = 5;

  
//
//  TRACKBARSTYLE class parts and states
//
const
  VSCLASS_TRACKBARSTYLE   = 'TRACKBARSTYLE';
  VSCLASS_TRACKBAR        = 'TRACKBAR';

type
  TRACKBARPARTS = Integer;
const
  TRACKBARPartFiller0 = 0;
  TKP_TRACK = 1;
  TKP_TRACKVERT = 2;
  TKP_THUMB = 3;
  TKP_THUMBBOTTOM = 4;
  TKP_THUMBTOP = 5;
  TKP_THUMBVERT = 6;
  TKP_THUMBLEFT = 7;
  TKP_THUMBRIGHT = 8;
  TKP_TICS = 9;
  TKP_TICSVERT = 10;

type
  TRACKBARSTATES = Integer;
const
  TRACKBARStateFiller0 = 0;
  TKS_NORMAL = 1;

type
  TRACKSTATES = Integer;
const
  TRACKStateFiller0 = 0;
  TRS_NORMAL = 1;

type
  TRACKVERTSTATES = Integer;
const
  TRACKVERTStateFiller0 = 0;
  TRVS_NORMAL = 1;

type
  THUMBSTATES = Integer;
const
  THUMBStateFiller0 = 0;
  TUS_NORMAL = 1;
  TUS_HOT = 2;
  TUS_PRESSED = 3;
  TUS_FOCUSED = 4;
  TUS_DISABLED = 5;

type
  THUMBBOTTOMSTATES = Integer;
const
  THUMBBOTTOMStateFiller0 = 0;
  TUBS_NORMAL = 1;
  TUBS_HOT = 2;
  TUBS_PRESSED = 3;
  TUBS_FOCUSED = 4;
  TUBS_DISABLED = 5;

type
  THUMBTOPSTATES = Integer;
const
  THUMBTOPStateFiller0 = 0;
  TUTS_NORMAL = 1;
  TUTS_HOT = 2;
  TUTS_PRESSED = 3;
  TUTS_FOCUSED = 4;
  TUTS_DISABLED = 5;

type
  THUMBVERTSTATES = Integer;
const
  THUMBVERTStateFiller0 = 0;
  TUVS_NORMAL = 1;
  TUVS_HOT = 2;
  TUVS_PRESSED = 3;
  TUVS_FOCUSED = 4;
  TUVS_DISABLED = 5;

type
  THUMBLEFTSTATES = Integer;
const
  THUMBLEFTStateFiller0 = 0;
  TUVLS_NORMAL = 1;
  TUVLS_HOT = 2;
  TUVLS_PRESSED = 3;
  TUVLS_FOCUSED = 4;
  TUVLS_DISABLED = 5;

type
  THUMBRIGHTSTATES = Integer;
const
  THUMBRIGHTStateFiller0 = 0;
  TUVRS_NORMAL = 1;
  TUVRS_HOT = 2;
  TUVRS_PRESSED = 3;
  TUVRS_FOCUSED = 4;
  TUVRS_DISABLED = 5;

type
  TICSSTATES = Integer;
const
  TICSStateFiller0 = 0;
  TSS_NORMAL = 1;

type
  TICSVERTSTATES = Integer;
const
  TICSVERTStateFiller0 = 0;
  TSVS_NORMAL = 1;

  
//
//  TOOLTIPSTYLE class parts and states
// 
const
  VSCLASS_TOOLTIPSTYLE    = 'TOOLTIPSTYLE';
  VSCLASS_TOOLTIP = 'TOOLTIP';

type
  TOOLTIPPARTS = Integer;
const
  TOOLTIPPartFiller0 = 0;
  TTP_STANDARD = 1;
  TTP_STANDARDTITLE = 2;
  TTP_BALLOON = 3;
  TTP_BALLOONTITLE = 4;
  TTP_CLOSE = 5;
  { For Windows >= Vista }
  TTP_BALLOONSTEM     = 6;

type
  CLOSESTATES = Integer;
const
  CLOSEStateFiller0 = 0;
  TTCS_NORMAL = 1;
  TTCS_HOT = 2;
  TTCS_PRESSED = 3;

type
  STANDARDSTATES = Integer;
const
  STANDARDStateFiller0 = 0;
  TTSS_NORMAL = 1;
  TTSS_LINK = 2;

type
  BALLOONSTATES = Integer;
const
  BALLOONStateFiller0 = 0;
  TTBS_NORMAL = 1;
  TTBS_LINK = 2;

type
  { For Windows >= Vista }
  BALLOONSTEMSTATES = Integer;
const
  TTBSS_POINTINGUPLEFTWALL     = 1;
  TTBSS_POINTINGUPCENTERED     = 2;
  TTBSS_POINTINGUPRIGHTWALL     = 3;
  TTBSS_POINTINGDOWNRIGHTWALL     = 4;
  TTBSS_POINTINGDOWNCENTERED     = 5;
  TTBSS_POINTINGDOWNLEFTWALL     = 6;

  
//
//  TREEVIEWSTYLE class parts and states
//
const
  VSCLASS_TREEVIEWSTYLE   = 'TREEVIEWSTYLE';
  VSCLASS_TREEVIEW        = 'TREEVIEW';

type
  TREEVIEWPARTS = Integer;
const
  TREEVIEWPartFiller0 = 0;
  TVP_TREEITEM = 1;
  TVP_GLYPH = 2;
  TVP_BRANCH = 3;
  { For Windows >= Vista }
  TVP_HOTGLYPH     = 4;

type
  TREEITEMSTATES = Integer;
const
  TREEITEMStateFiller0 = 0;
  TREIS_NORMAL = 1;
  TREIS_HOT = 2;
  TREIS_SELECTED = 3;
  TREIS_DISABLED = 4;
  TREIS_SELECTEDNOTFOCUS = 5;
  { For Windows >= Vista }
  TREIS_HOTSELECTED     = 6;

type
  GLYPHSTATES = Integer;
const
  GLYPHStateFiller0 = 0;
  GLPS_CLOSED = 1;
  GLPS_OPENED = 2;

type
  { For Windows >= Vista }
  HOTGLYPHSTATES = Integer;
const
  HGLPS_CLOSED     = 1;
  HGLPS_OPENED     = 2;

  
//
//  SPINSTYLE class parts and states
//
const
  VSCLASS_SPINSTYLE       = 'SPINSTYLE';
  VSCLASS_SPIN    = 'SPIN';

type
  SPINPARTS = Integer;
const
  SPINPartFiller0 = 0;
  SPNP_UP = 1;
  SPNP_DOWN = 2;
  SPNP_UPHORZ = 3;
  SPNP_DOWNHORZ = 4;

type
  UPSTATES = Integer;
const
  UPStateFiller0 = 0;
  UPS_NORMAL = 1;
  UPS_HOT = 2;
  UPS_PRESSED = 3;
  UPS_DISABLED = 4;

type
  DOWNSTATES = Integer;
const
  DOWNStateFiller0 = 0;
  DNS_NORMAL = 1;
  DNS_HOT = 2;
  DNS_PRESSED = 3;
  DNS_DISABLED = 4;

type
  UPHORZSTATES = Integer;
const
  UPHORZStateFiller0 = 0;
  UPHZS_NORMAL = 1;
  UPHZS_HOT = 2;
  UPHZS_PRESSED = 3;
  UPHZS_DISABLED = 4;

type
  DOWNHORZSTATES = Integer;
const
  DOWNHORZStateFiller0 = 0;
  DNHZS_NORMAL = 1;
  DNHZS_HOT = 2;
  DNHZS_PRESSED = 3;
  DNHZS_DISABLED = 4;

  
//
//  PAGE class parts and states
//
const
  VSCLASS_PAGE    = 'PAGE';

type
  PAGEPARTS = Integer;
const
  PAGEPartFiller0 = 0;
  PGRP_UP = 1;
  PGRP_DOWN = 2;
  PGRP_UPHORZ = 3;
  PGRP_DOWNHORZ = 4;

  
//
//  SCROLLBARSTYLE class parts and states
//
const
  VSCLASS_SCROLLBARSTYLE  = 'SCROLLBARSTYLE';
  VSCLASS_SCROLLBAR       = 'SCROLLBAR';

type
  SCROLLBARPARTS = Integer;
const
  SCROLLBARPartFiller0 = 0;
  SBP_ARROWBTN = 1;
  SBP_THUMBBTNHORZ = 2;
  SBP_THUMBBTNVERT = 3;
  SBP_LOWERTRACKHORZ = 4;
  SBP_UPPERTRACKHORZ = 5;
  SBP_LOWERTRACKVERT = 6;
  SBP_UPPERTRACKVERT = 7;
  SBP_GRIPPERHORZ = 8;
  SBP_GRIPPERVERT = 9;
  SBP_SIZEBOX = 10;

type
  ARROWBTNSTATES = Integer;
const
  ARROWBTNStateFiller0 = 0;
  ABS_UPNORMAL = 1;
  ABS_UPHOT = 2;
  ABS_UPPRESSED = 3;
  ABS_UPDISABLED = 4;
  ABS_DOWNNORMAL = 5;
  ABS_DOWNHOT = 6;
  ABS_DOWNPRESSED = 7;
  ABS_DOWNDISABLED = 8;
  ABS_LEFTNORMAL = 9;
  ABS_LEFTHOT = 10;
  ABS_LEFTPRESSED = 11;
  ABS_LEFTDISABLED = 12;
  ABS_RIGHTNORMAL = 13;
  ABS_RIGHTHOT = 14;
  ABS_RIGHTPRESSED = 15;
  ABS_RIGHTDISABLED = 16;
  { For Windows >= Vista }
  ABS_UPHOVER     = 17;
  ABS_DOWNHOVER     = 18;
  ABS_LEFTHOVER     = 19;
  ABS_RIGHTHOVER     = 20;

type
  SCROLLBARSTATES = Integer;
const
  SCROLLBARStateFiller0 = 0;
  SCRBS_NORMAL = 1;
  SCRBS_HOT = 2;
  SCRBS_PRESSED = 3;
  SCRBS_DISABLED = 4;
  { For Windows >= Vista }
  SCRBS_HOVER     = 5;

type
  SIZEBOXSTATES = Integer;
const
  SIZEBOXStateFiller0 = 0;
  SZB_RIGHTALIGN = 1;
  SZB_LEFTALIGN = 2;
  { For Windows >= Vista }
  SZB_TOPRIGHTALIGN     = 3;
  SZB_TOPLEFTALIGN     = 4;
  SZB_HALFBOTTOMRIGHTALIGN     = 5;
  SZB_HALFBOTTOMLEFTALIGN     = 6;
  SZB_HALFTOPRIGHTALIGN     = 7;
  SZB_HALFTOPLEFTALIGN     = 8;

  
//
//  EDITSTYLE class parts and states
//
const
  VSCLASS_EDITSTYLE       = 'EDITSTYLE';
  VSCLASS_EDIT    = 'EDIT';

type
  EDITPARTS = Integer;
const
  EDITPartFiller0 = 0;
  EP_EDITTEXT = 1;
  EP_CARET = 2;
  { For Windows >= Vista }
  EP_BACKGROUND     = 3;
  EP_PASSWORD     = 4;
  EP_BACKGROUNDWITHBORDER     = 5;
  EP_EDITBORDER_NOSCROLL     = 6;
  EP_EDITBORDER_HSCROLL     = 7;
  EP_EDITBORDER_VSCROLL     = 8;
  EP_EDITBORDER_HVSCROLL     = 9;

type
  EDITTEXTSTATES = Integer;
const
  EDITTEXTStateFiller0 = 0;
  ETS_NORMAL = 1;
  ETS_HOT = 2;
  ETS_SELECTED = 3;
  ETS_DISABLED = 4;
  ETS_FOCUSED = 5;
  ETS_READONLY = 6;
  ETS_ASSIST = 7;
  { For Windows >= Vista }
  ETS_CUEBANNER     = 8;

type
  { For Windows >= Vista }
  BACKGROUNDSTATES = Integer;
const
  EBS_NORMAL     = 1;
  EBS_HOT     = 2;
  EBS_DISABLED     = 3;
  EBS_FOCUSED     = 4;
  EBS_READONLY     = 5;
  EBS_ASSIST     = 6;

type
  { For Windows >= Vista }
  BACKGROUNDWITHBORDERSTATES = Integer;
const
  EBWBS_NORMAL     = 1;
  EBWBS_HOT     = 2;
  EBWBS_DISABLED     = 3;
  EBWBS_FOCUSED     = 4;

type
  { For Windows >= Vista }
  EDITBORDER_NOSCROLLSTATES = Integer;
const
  EPSN_NORMAL     = 1;
  EPSN_HOT     = 2;
  EPSN_FOCUSED     = 3;
  EPSN_DISABLED     = 4;

type
  { For Windows >= Vista }
  EDITBORDER_HSCROLLSTATES = Integer;
const
  EPSH_NORMAL     = 1;
  EPSH_HOT     = 2;
  EPSH_FOCUSED     = 3;
  EPSH_DISABLED     = 4;

type
  { For Windows >= Vista }
  EDITBORDER_VSCROLLSTATES = Integer;
const
  EPSV_NORMAL     = 1;
  EPSV_HOT     = 2;
  EPSV_FOCUSED     = 3;
  EPSV_DISABLED     = 4;

type
  { For Windows >= Vista }
  EDITBORDER_HVSCROLLSTATES = Integer;
const
  EPSHV_NORMAL     = 1;
  EPSHV_HOT     = 2;
  EPSHV_FOCUSED     = 3;
  EPSHV_DISABLED     = 4;


//
//  COMBOBOXSTYLE class parts and states
//
const
  VSCLASS_COMBOBOXSTYLE   = 'COMBOBOXSTYLE';
  VSCLASS_COMBOBOX        = 'COMBOBOX';

type
  COMBOBOXPARTS = Integer;
const
  COMBOBOXPartFiller0 = 0;
  CP_DROPDOWNBUTTON = 1;
  { For Windows >= Vista }
  CP_BACKGROUND     = 2;
  CP_TRANSPARENTBACKGROUND     = 3;
  CP_BORDER     = 4;
  CP_READONLY     = 5;
  CP_DROPDOWNBUTTONRIGHT     = 6;
  CP_DROPDOWNBUTTONLEFT     = 7;
  CP_CUEBANNER     = 8;

type
  COMBOBOXSTATES = Integer;
const
  COMBOBOXStateFiller0 = 0;
  CBXS_NORMAL = 1;
  CBXS_HOT = 2;
  CBXS_PRESSED = 3;
  CBXS_DISABLED = 4;

type
  { For Windows >= Vista }
  DROPDOWNBUTTONRIGHTSTATES = Integer;
const
  CBXSR_NORMAL     = 1;
  CBXSR_HOT     = 2;
  CBXSR_PRESSED     = 3;
  CBXSR_DISABLED     = 4;

type
  { For Windows >= Vista }
  DROPDOWNBUTTONLEFTSTATES = Integer;
const
  CBXSL_NORMAL     = 1;
  CBXSL_HOT     = 2;
  CBXSL_PRESSED     = 3;
  CBXSL_DISABLED     = 4;

type
  { For Windows >= Vista }
  TRANSPARENTBACKGROUNDSTATES = Integer;
const
  CBTBS_NORMAL     = 1;
  CBTBS_HOT     = 2;
  CBTBS_DISABLED     = 3;
  CBTBS_FOCUSED     = 4;

type
  { For Windows >= Vista }
  BORDERSTATES = Integer;
const
  CBB_NORMAL     = 1;
  CBB_HOT     = 2;
  CBB_FOCUSED     = 3;
  CBB_DISABLED     = 4;

type
  { For Windows >= Vista }
  READONLYSTATES = Integer;
const
  CBRO_NORMAL     = 1;
  CBRO_HOT     = 2;
  CBRO_PRESSED     = 3;
  CBRO_DISABLED     = 4;

type
  { For Windows >= Vista }
  CUEBANNERSTATES = Integer;
const
  CBCB_NORMAL     = 1;
  CBCB_HOT     = 2;
  CBCB_PRESSED     = 3;
  CBCB_DISABLED     = 4;

  
//
//  CLOCK class parts and states
//
const
  VSCLASS_CLOCK   = 'CLOCK';

type
  CLOCKPARTS = Integer;
const
  CLOCKPartFiller0 = 0;
  CLP_TIME = 1;

type
  CLOCKSTATES = Integer;
const
  CLOCKStateFiller0 = 0;
  CLS_NORMAL = 1;

  
//
//  TRAYNOTIFY class parts and states
//
const
  VSCLASS_TRAYNOTIFY      = 'TRAYNOTIFY';

type
  TRAYNOTIFYPARTS = Integer;
const
  TRAYNOTIFYPartFiller0 = 0;
  TNP_BACKGROUND = 1;
  TNP_ANIMBACKGROUND = 2;

  
//
//  TASKBAR class parts and states
//
const
  VSCLASS_TASKBAR = 'TASKBAR';

type
  TASKBARPARTS = Integer;
const
  TASKBARPartFiller0 = 0;
  TBP_BACKGROUNDBOTTOM = 1;
  TBP_BACKGROUNDRIGHT = 2;
  TBP_BACKGROUNDTOP = 3;
  TBP_BACKGROUNDLEFT = 4;
  TBP_SIZINGBARBOTTOM = 5;
  TBP_SIZINGBARRIGHT = 6;
  TBP_SIZINGBARTOP = 7;
  TBP_SIZINGBARLEFT = 8;

  
//
//  TASKBAND class parts and states
//
const
  VSCLASS_TASKBAND        = 'TASKBAND';

type
  TASKBANDPARTS = Integer;
const
  TASKBANDPartFiller0 = 0;
  TDP_GROUPCOUNT = 1;
  TDP_FLASHBUTTON = 2;
  TDP_FLASHBUTTONGROUPMENU = 3;

  
//
//  STARTPANEL class parts and states
//
const
  VSCLASS_STARTPANEL      = 'STARTPANEL';

type
  STARTPANELPARTS = Integer;
const
  STARTPANELPartFiller0 = 0;
  SPP_USERPANE = 1;
  SPP_MOREPROGRAMS = 2;
  SPP_MOREPROGRAMSARROW = 3;
  SPP_PROGLIST = 4;
  SPP_PROGLISTSEPARATOR = 5;
  SPP_PLACESLIST = 6;
  SPP_PLACESLISTSEPARATOR = 7;
  SPP_LOGOFF = 8;
  SPP_LOGOFFBUTTONS = 9;
  SPP_USERPICTURE = 10;
  SPP_PREVIEW = 11;
  { For Windows >= Vista }
  SPP_MOREPROGRAMSTAB     = 12;
  SPP_NSCHOST     = 13;
  SPP_SOFTWAREEXPLORER     = 14;
  SPP_OPENBOX     = 15;
  SPP_SEARCHVIEW     = 16;
  SPP_MOREPROGRAMSARROWBACK     = 17;
  SPP_TOPMATCH     = 18;
  SPP_LOGOFFSPLITBUTTONDROPDOWN     = 19;

type
  { For Windows >= Vista }
  MOREPROGRAMSTABSTATES = Integer;
const
  SPMPT_NORMAL     = 1;
  SPMPT_HOT     = 2;
  SPMPT_SELECTED     = 3;
  SPMPT_DISABLED     = 4;
  SPMPT_FOCUSED     = 5;
  
type
  MOREPROGRAMSARROWSTATES = Integer;
const
  MOREPROGRAMSARROWStateFiller0 = 0;
  SPS_NORMAL = 1;
  SPS_HOT = 2;
  SPS_PRESSED = 3;

type
  { For Windows >= Vista }
  SOFTWAREEXPLORERSTATES = Integer;
const
  SPSE_NORMAL     = 1;
  SPSE_HOT     = 2;
  SPSE_SELECTED     = 3;
  SPSE_DISABLED     = 4;
  SPSE_FOCUSED     = 5;

type
  { For Windows >= Vista }
  OPENBOXSTATES = Integer;
const
  SPOB_NORMAL     = 1;
  SPOB_HOT     = 2;
  SPOB_SELECTED     = 3;
  SPOB_DISABLED     = 4;
  SPOB_FOCUSED     = 5;

type
  { For Windows >= Vista }
  MOREPROGRAMSARROWBACKSTATES = Integer;
const
  SPSB_NORMAL     = 1;
  SPSB_HOT     = 2;
  SPSB_PRESSED     = 3;

type
  LOGOFFBUTTONSSTATES = Integer;
const
  LOGOFFBUTTONSStateFiller0 = 0;
  SPLS_NORMAL = 1;
  SPLS_HOT = 2;
  SPLS_PRESSED = 3;

  
//
//  EXPLORERBARSTYLE class parts and states
//
const
  VSCLASS_EXPLORERBARSTYLE        = 'EXPLORERBARSTYLE';
  VSCLASS_EXPLORERBAR     = 'EXPLORERBAR';

type
  EXPLORERBARPARTS = Integer;
const
  EXPLORERBARPartFiller0 = 0;
  EBP_HEADERBACKGROUND = 1;
  EBP_HEADERCLOSE = 2;
  EBP_HEADERPIN = 3;
  EBP_IEBARMENU = 4;
  EBP_NORMALGROUPBACKGROUND = 5;
  EBP_NORMALGROUPCOLLAPSE = 6;
  EBP_NORMALGROUPEXPAND = 7;
  EBP_NORMALGROUPHEAD = 8;
  EBP_SPECIALGROUPBACKGROUND = 9;
  EBP_SPECIALGROUPCOLLAPSE = 10;
  EBP_SPECIALGROUPEXPAND = 11;
  EBP_SPECIALGROUPHEAD = 12;

type
  HEADERCLOSESTATES = Integer;
const
  HEADERCLOSEStateFiller0 = 0;
  EBHC_NORMAL = 1;
  EBHC_HOT = 2;
  EBHC_PRESSED = 3;

type
  HEADERPINSTATES = Integer;
const
  HEADERPINStateFiller0 = 0;
  EBHP_NORMAL = 1;
  EBHP_HOT = 2;
  EBHP_PRESSED = 3;
  EBHP_SELECTEDNORMAL = 4;
  EBHP_SELECTEDHOT = 5;
  EBHP_SELECTEDPRESSED = 6;

type
  IEBARMENUSTATES = Integer;
const
  IEBARMENUStateFiller0 = 0;
  EBM_NORMAL = 1;
  EBM_HOT = 2;
  EBM_PRESSED = 3;

type
  NORMALGROUPCOLLAPSESTATES = Integer;
const
  NORMALGROUPCOLLAPSEStateFiller0 = 0;
  EBNGC_NORMAL = 1;
  EBNGC_HOT = 2;
  EBNGC_PRESSED = 3;

type
  NORMALGROUPEXPANDSTATES = Integer;
const
  NORMALGROUPEXPANDStateFiller0 = 0;
  EBNGE_NORMAL = 1;
  EBNGE_HOT = 2;
  EBNGE_PRESSED = 3;

type
  SPECIALGROUPCOLLAPSESTATES = Integer;
const
  SPECIALGROUPCOLLAPSEStateFiller0 = 0;
  EBSGC_NORMAL = 1;
  EBSGC_HOT = 2;
  EBSGC_PRESSED = 3;

type
  SPECIALGROUPEXPANDSTATES = Integer;
const
  SPECIALGROUPEXPANDStateFiller0 = 0;
  EBSGE_NORMAL = 1;
  EBSGE_HOT = 2;
  EBSGE_PRESSED = 3;

  
//
//  MENUBAND class parts and states
//
const
  VSCLASS_MENUBAND        = 'MENUBAND';

type
  MENUBANDPARTS = Integer;
const
  MENUBANDPartFiller0 = 0;
  MDP_NEWAPPBUTTON = 1;
  MDP_SEPERATOR = 2;

type
  MENUBANDSTATES = Integer;
const
  MENUBANDStateFiller0 = 0;
  MDS_NORMAL = 1;
  MDS_HOT = 2;
  MDS_PRESSED = 3;
  MDS_DISABLED = 4;
  MDS_CHECKED = 5;
  MDS_HOTCHECKED = 6;


// ** The following declarations require Windows >= Vista **


//
//  AEROWIZARDSTYLE class parts and states
//
const
  VSCLASS_AEROWIZARDSTYLE = 'AEROWIZARDSTYLE';
  VSCLASS_AEROWIZARD      = 'AEROWIZARD';

type
  AEROWIZARDPARTS = Integer;
const
  AW_TITLEBAR     = 1;
  AW_HEADERAREA     = 2;
  AW_CONTENTAREA     = 3;
  AW_COMMANDAREA     = 4;
  AW_BUTTON     = 5;

type
  AEROWIZARDSTYLEPARTS = AEROWIZARDPARTS;

type
  TITLEBARSTATES = Integer;
const
  AW_S_TITLEBAR_ACTIVE     = 1;
  AW_S_TITLEBAR_INACTIVE     = 2;

type
  HEADERAREASTATES = Integer;
const
  AW_S_HEADERAREA_NOMARGIN     = 1;

type
  CONTENTAREASTATES = Integer;
const
  AW_S_CONTENTAREA_NOMARGIN     = 1;


//
//  COMMUNICATIONSSTYLE class parts and states
//
const
  VSCLASS_COMMUNICATIONSSTYLE     = 'COMMUNICATIONSSTYLE';
  VSCLASS_COMMUNICATIONS  = 'COMMUNICATIONS';

type
  COMMUNICATIONSPARTS = Integer;
const
  CSST_TAB     = 1;

type
  TABSTATES = Integer;
const
  CSTB_NORMAL     = 1;
  CSTB_HOT     = 2;
  CSTB_SELECTED     = 3;


//
//  CONTROLPANELSTYLE class parts and states
//
const
  VSCLASS_CONTROLPANELSTYLE       = 'CONTROLPANELSTYLE';
  VSCLASS_CONTROLPANEL    = 'CONTROLPANEL';

type
  CONTROLPANELPARTS = Integer;
const
  CPANEL_NAVIGATIONPANE     = 1;
  CPANEL_CONTENTPANE     = 2;
  CPANEL_NAVIGATIONPANELABEL     = 3;
  CPANEL_CONTENTPANELABEL     = 4;
  CPANEL_TITLE     = 5;
  CPANEL_BODYTEXT     = 6;
  CPANEL_HELPLINK     = 7;
  CPANEL_TASKLINK     = 8;
  CPANEL_GROUPTEXT     = 9;
  CPANEL_CONTENTLINK     = 10;
  CPANEL_SECTIONTITLELINK     = 11;
  CPANEL_LARGECOMMANDAREA     = 12;
  CPANEL_SMALLCOMMANDAREA     = 13;
  CPANEL_BUTTON     = 14;
  CPANEL_MESSAGETEXT     = 15;
  CPANEL_NAVIGATIONPANELINE     = 16;
  CPANEL_CONTENTPANELINE     = 17;
  CPANEL_BANNERAREA     = 18;
  CPANEL_BODYTITLE     = 19;

type
  HELPLINKSTATES = Integer;
const
  CPHL_NORMAL     = 1;
  CPHL_HOT     = 2;
  CPHL_PRESSED     = 3;
  CPHL_DISABLED     = 4;

type
  TASKLINKSTATES = Integer;
const
  CPTL_NORMAL     = 1;
  CPTL_HOT     = 2;
  CPTL_PRESSED     = 3;
  CPTL_DISABLED     = 4;
  CPTL_PAGE     = 5;

type
  CONTENTLINKSTATES = Integer;
const
  CPCL_NORMAL     = 1;
  CPCL_HOT     = 2;
  CPCL_PRESSED     = 3;
  CPCL_DISABLED     = 4;

type
  SECTIONTITLELINKSTATES = Integer;
const
  CPSTL_NORMAL     = 1;
  CPSTL_HOT     = 2;


//
//  DATEPICKERSTYLE class parts and states
//
const
  VSCLASS_DATEPICKERSTYLE = 'DATEPICKERSTYLE';
  VSCLASS_DATEPICKER      = 'DATEPICKER';

type
  DATEPICKERPARTS = Integer; 
const
  DP_DATETEXT     = 1; 
  DP_DATEBORDER     = 2; 
  DP_SHOWCALENDARBUTTONRIGHT     = 3; 

type
  DATETEXTSTATES = Integer;
const
  DPDT_NORMAL     = 1;
  DPDT_DISABLED     = 2;
  DPDT_SELECTED     = 3;

type
  DATEBORDERSTATES = Integer;
const
  DPDB_NORMAL     = 1;
  DPDB_HOT     = 2;
  DPDB_FOCUSED     = 3;
  DPDB_DISABLED     = 4;

type
  SHOWCALENDARBUTTONRIGHTSTATES = Integer;
const
  DPSCBR_NORMAL     = 1;
  DPSCBR_HOT     = 2;
  DPSCBR_PRESSED     = 3;
  DPSCBR_DISABLED     = 4;


// 
//  DRAGDROPSTYLE class parts and states
//
const
  VSCLASS_DRAGDROPSTYLE   = 'DRAGDROPSTYLE';
  VSCLASS_DRAGDROP        = 'DRAGDROP';

type
  DRAGDROPPARTS = Integer;
const
  DD_COPY     = 1;
  DD_MOVE     = 2;
  DD_UPDATEMETADATA     = 3;
  DD_CREATELINK     = 4;
  DD_WARNING     = 5;
  DD_NONE     = 6;
  DD_IMAGEBG     = 7;
  DD_TEXTBG     = 8;

type
  COPYSTATES = Integer;
const
  DDCOPY_HIGHLIGHT     = 1;
  DDCOPY_NOHIGHLIGHT     = 2;

type
  MOVESTATES = Integer;
const
  DDMOVE_HIGHLIGHT     = 1;
  DDMOVE_NOHIGHLIGHT     = 2;

type
  UPDATEMETADATASTATES = Integer;
const
  DDUPDATEMETADATA_HIGHLIGHT     = 1;
  DDUPDATEMETADATA_NOHIGHLIGHT     = 2;

type
  CREATELINKSTATES = Integer;
const
  DDCREATELINK_HIGHLIGHT     = 1;
  DDCREATELINK_NOHIGHLIGHT     = 2;

type
  WARNINGSTATES = Integer;
const
  DDWARNING_HIGHLIGHT     = 1;
  DDWARNING_NOHIGHLIGHT     = 2;

type
  NONESTATES = Integer;
const
  DDNONE_HIGHLIGHT     = 1;
  DDNONE_NOHIGHLIGHT     = 2;


//
//  FLYOUTSTYLE class parts and states
//
const 
  VSCLASS_FLYOUTSTYLE     = 'FLYOUTSTYLE'; 
  VSCLASS_FLYOUT  = 'FLYOUT'; 

type
  FLYOUTPARTS = Integer; 
const
  FLYOUT_HEADER     = 1; 
  FLYOUT_BODY     = 2; 
  FLYOUT_LABEL     = 3; 
  FLYOUT_LINK     = 4; 
  FLYOUT_DIVIDER     = 5; 
  FLYOUT_WINDOW     = 6; 
  FLYOUT_LINKAREA     = 7; 
  FLYOUT_LINKHEADER     = 8; 

type
  LABELSTATES = Integer;
const
  FLS_NORMAL     = 1;
  FLS_SELECTED     = 2;
  FLS_EMPHASIZED     = 3;
  FLS_DISABLED     = 4;

type
  LINKSTATES = Integer;
const
  FLYOUTLINK_NORMAL     = 1;
  FLYOUTLINK_HOVER     = 2;

type
  BODYSTATES = Integer;
const
  FBS_NORMAL     = 1;
  FBS_EMPHASIZED     = 2;

type
  LINKHEADERSTATES = Integer;
const
  FLH_NORMAL     = 1;
  FLH_HOVER     = 2;


//
//  LISTBOXSTYLE class parts and states
//
const
  VSCLASS_LISTBOXSTYLE    = 'LISTBOXSTYLE';
  VSCLASS_LISTBOX = 'LISTBOX';

type
  LISTBOXPARTS = Integer;
const
  LBCP_BORDER_HSCROLL     = 1;
  LBCP_BORDER_HVSCROLL     = 2;
  LBCP_BORDER_NOSCROLL     = 3;
  LBCP_BORDER_VSCROLL     = 4;
  LBCP_ITEM     = 5;

type
  BORDER_HSCROLLSTATES = Integer;
const
  LBPSH_NORMAL     = 1;
  LBPSH_FOCUSED     = 2;
  LBPSH_HOT     = 3;
  LBPSH_DISABLED     = 4;

type
  BORDER_HVSCROLLSTATES = Integer;
const
  LBPSHV_NORMAL     = 1;
  LBPSHV_FOCUSED     = 2;
  LBPSHV_HOT     = 3;
  LBPSHV_DISABLED     = 4;

type
  BORDER_NOSCROLLSTATES = Integer;
const
  LBPSN_NORMAL     = 1;
  LBPSN_FOCUSED     = 2;
  LBPSN_HOT     = 3;
  LBPSN_DISABLED     = 4;

type
  BORDER_VSCROLLSTATES = Integer;
const
  LBPSV_NORMAL     = 1;
  LBPSV_FOCUSED     = 2;
  LBPSV_HOT     = 3;
  LBPSV_DISABLED     = 4;

type
  ITEMSTATES = Integer;
const
  LBPSI_HOT     = 1;
  LBPSI_HOTSELECTED     = 2;
  LBPSI_SELECTED     = 3;
  LBPSI_SELECTEDNOTFOCUS     = 4;


//
//  NAVIGATION class parts and states
//
const
  VSCLASS_NAVIGATION      = 'NAVIGATION';

type
  NAVIGATIONPARTS = Integer;
const
  NAV_BACKBUTTON     = 1;
  NAV_FORWARDBUTTON     = 2;
  NAV_MENUBUTTON     = 3;

type
  NAV_BACKBUTTONSTATES = Integer;
const
  NAV_BB_NORMAL     = 1;
  NAV_BB_HOT     = 2;
  NAV_BB_PRESSED     = 3;
  NAV_BB_DISABLED     = 4;

type
  NAV_FORWARDBUTTONSTATES = Integer;
const
  NAV_FB_NORMAL     = 1;
  NAV_FB_HOT     = 2;
  NAV_FB_PRESSED     = 3;
  NAV_FB_DISABLED     = 4;

type
  NAV_MENUBUTTONSTATES = Integer;
const
  NAV_MB_NORMAL     = 1;
  NAV_MB_HOT     = 2;
  NAV_MB_PRESSED     = 3;
  NAV_MB_DISABLED     = 4;


// 
//  TASKDIALOGSTYLE class parts and states
//
const 
  VSCLASS_TASKDIALOGSTYLE = 'TASKDIALOGSTYLE'; 
  VSCLASS_TASKDIALOG      = 'TASKDIALOG'; 

type
  TASKDIALOGPARTS = Integer; 
const
  TDLG_PRIMARYPANEL     = 1; 
  TDLG_MAININSTRUCTIONPANE     = 2; 
  TDLG_MAINICON     = 3; 
  TDLG_CONTENTPANE     = 4; 
  TDLG_CONTENTICON     = 5; 
  TDLG_EXPANDEDCONTENT     = 6;
  TDLG_COMMANDLINKPANE     = 7; 
  TDLG_SECONDARYPANEL     = 8; 
  TDLG_CONTROLPANE     = 9; 
  TDLG_BUTTONSECTION     = 10; 
  TDLG_BUTTONWRAPPER     = 11; 
  TDLG_EXPANDOTEXT     = 12; 
  TDLG_EXPANDOBUTTON     = 13; 
  TDLG_VERIFICATIONTEXT     = 14; 
  TDLG_FOOTNOTEPANE     = 15; 
  TDLG_FOOTNOTEAREA     = 16; 
  TDLG_FOOTNOTESEPARATOR     = 17; 
  TDLG_EXPANDEDFOOTERAREA     = 18;
  TDLG_PROGRESSBAR     = 19; 
  TDLG_IMAGEALIGNMENT     = 20; 
  TDLG_RADIOBUTTONPANE     = 21; 

type
  CONTENTPANESTATES = Integer;
const
  TDLGCPS_STANDALONE     = 1;

type
  EXPANDOBUTTONSTATES = Integer;
const
  TDLGEBS_NORMAL     = 1;
  TDLGEBS_HOVER     = 2;
  TDLGEBS_PRESSED     = 3;
  TDLGEBS_EXPANDEDNORMAL     = 4;
  TDLGEBS_EXPANDEDHOVER     = 5;
  TDLGEBS_EXPANDEDPRESSED     = 6;


//
//  TEXTSTYLE class parts and states
//
const 
  VSCLASS_TEXTSTYLE       = 'TEXTSTYLE'; 

type
  TEXTSTYLEPARTS = Integer;
const
  TEXT_MAININSTRUCTION     = 1; 
  TEXT_INSTRUCTION     = 2; 
  TEXT_BODYTITLE     = 3; 
  TEXT_BODYTEXT     = 4; 
  TEXT_SECONDARYTEXT     = 5; 
  TEXT_HYPERLINKTEXT     = 6; 
  TEXT_EXPANDED     = 7; 
  TEXT_LABEL     = 8; 
  TEXT_CONTROLLABEL     = 9; 

type
  HYPERLINKTEXTSTATES = Integer;
const
  TS_HYPERLINK_NORMAL     = 1;
  TS_HYPERLINK_HOT     = 2;
  TS_HYPERLINK_PRESSED     = 3;
  TS_HYPERLINK_DISABLED     = 4;

type
  CONTROLLABELSTATES = Integer;
const
  TS_CONTROLLABEL_NORMAL     = 1;
  TS_CONTROLLABEL_DISABLED     = 2;


//
//  LINK class parts and states
//
const
  VSCLASS_LINK    = 'LINK';

type
  LINKPARTS = Integer;
const
  LP_HYPERLINK     = 1;

type
  HYPERLINKSTATES = Integer;
const
  HLS_NORMALTEXT     = 1;
  HLS_LINKTEXT     = 2;

  
//
//  EMPTYMARKUP class parts and states
//
const
  VSCLASS_EMPTYMARKUP     = 'EMPTYMARKUP';

type
  EMPTYMARKUPPARTS = Integer;
const
  EMP_MARKUPTEXT     = 1;

type
  MARKUPTEXTSTATES = Integer;
const
  EMT_NORMALTEXT     = 1;
  EMT_LINKTEXT     = 2;

  
//
//  STATIC class parts and states
//
const
  VSCLASS_STATIC  = 'STATIC';

type
  STATICPARTS = Integer;
const
  STAT_TEXT     = 1;

  
//
//  MONTHCAL class parts and states
//
const
  VSCLASS_MONTHCAL        = 'MONTHCAL';

type
  MONTHCALPARTS = Integer;
const
  MC_BACKGROUND     = 1;
  MC_BORDERS     = 2;
  MC_GRIDBACKGROUND     = 3;
  MC_COLHEADERSPLITTER     = 4;
  MC_GRIDCELLBACKGROUND     = 5;
  MC_GRIDCELL     = 6;
  MC_GRIDCELLUPPER     = 7;
  MC_TRAILINGGRIDCELL     = 8;
  MC_TRAILINGGRIDCELLUPPER     = 9;
  MC_NAVNEXT     = 10;
  MC_NAVPREV     = 11;

type
  GRIDCELLBACKGROUNDSTATES = Integer;
const
  MCGCB_SELECTED     = 1;
  MCGCB_HOT     = 2;
  MCGCB_SELECTEDHOT     = 3;
  MCGCB_SELECTEDNOTFOCUSED     = 4;
  MCGCB_TODAY     = 5;

type
  GRIDCELLSTATES = Integer;
const
  MCGC_HOT     = 1;
  MCGC_HASSTATE     = 2;
  MCGC_HASSTATEHOT     = 3;
  MCGC_TODAY     = 4;

type
  GRIDCELLUPPERSTATES = Integer;
const
  MCGCU_HOT     = 1;
  MCGCU_HASSTATE     = 2;
  MCGCU_HASSTATEHOT     = 3;

type
  TRAILINGGRIDCELLSTATES = Integer;
const
  MCTGC_HOT     = 1;
  MCTGC_HASSTATE     = 2;
  MCTGC_HASSTATEHOT     = 3;
  MCTGC_TODAY     = 4;

type
  TRAILINGGRIDCELLUPPERSTATES = Integer;
const
  MCTGCU_HOT     = 1;
  MCTGCU_HASSTATE     = 2;
  MCTGCU_HASSTATEHOT     = 3;

type
  NAVNEXTSTATES = Integer;
const
  MCNN_NORMAL     = 1;
  MCNN_HOT     = 2;
  MCNN_PRESSED     = 3;
  MCNN_DISABLED     = 4;

type
  NAVPREVSTATES = Integer;
const
  MCNP_NORMAL     = 1;
  MCNP_HOT     = 2;
  MCNP_PRESSED     = 3;
  MCNP_DISABLED     = 4;

const
  OTD_FORCE_RECT_SIZING   = $00000001;          // make all parts size to rect
  OTD_NONCLIENT           = $00000002;          // set if hTheme to be used for nonclient area
  OTD_VALIDBITS           = OTD_FORCE_RECT_SIZING or OTD_NONCLIENT;

  
// ---------------------------------------------------------------------------
//  OpenThemeDataEx     - Open the theme data for the specified HWND and
//                        semi-colon separated list of class names.
// 
//                        OpenThemeData() will try each class name, one at
//                        a time, and use the first matching theme info
//                        found.  If a match is found, a theme handle
//                        to the data is returned.  If no match is found,
//                        a "NULL" handle is returned.
// 
//                        When the window is destroyed or a WM_THEMECHANGED
//                        msg is received, "CloseThemeData()" should be
//                        called to close the theme handle.
// 
//  hwnd                - window handle of the control/window to be themed
// 
//  pszClassList        - class name (or list of names) to match to theme data
//                        section.  if the list contains more than one name,
//                        the names are tested one at a time for a match. 
//                        If a match is found, OpenThemeData() returns a
//                        theme handle associated with the matching class.
//                        This param is a list (instead of just a single
//                        class name) to provide the class an opportunity
//                        to get the "best" match between the class and
//                        the current theme.  For example, a button might
//                        pass L"OkButton, Button" if its ID=ID_OK.  If
//                        the current theme has an entry for OkButton,
//                        that will be used.  Otherwise, we fall back on
//                        the normal Button entry.
// 
//  dwFlags              - allows certain overrides of std features
//                         (see OTD_XXX defines above)
// ---------------------------------------------------------------------------
function OpenThemeDataEx(hwnd: HWND; pszClassList: LPCWSTR;
  dwFlags: DWORD): HTHEME;

// ---- bits used in dwFlags of DTBGOPTS ----
const
  DTBG_CLIPRECT           = $00000001;  // rcClip has been specified
  DTBG_DRAWSOLID          = $00000002;  // DEPRECATED: draw transparent/alpha images as solid
  DTBG_OMITBORDER         = $00000004;  // don't draw border of part
  DTBG_OMITCONTENT        = $00000008;  // don't draw content area of part
  DTBG_COMPUTINGREGION    = $00000010;  // TRUE if calling to compute region
  DTBG_MIRRORDC           = $00000020;  // assume the hdc is mirrorred and
                                            // flip images as appropriate (currently
                                            // only supported for bgtype=imagefile)
  DTBG_NOMIRROR           = $00000040;  // don't mirror the output, overrides everything else
  DTBG_VALIDBITS          = DTBG_CLIPRECT or
                            DTBG_DRAWSOLID or
                            DTBG_OMITBORDER or
                            DTBG_OMITCONTENT or
                            DTBG_COMPUTINGREGION or
                            DTBG_MIRRORDC or
                            DTBG_NOMIRROR;

type
  PDTBGOPTS = ^DTBGOPTS;
  DTBGOPTS = packed record
    dwSize: DWORD;          // size of the struct
    dwFlags: DWORD;         // which options have been specified
    rcClip: TRect;          // clipping rectangle
  end;
  _DTBGOPTS = DTBGOPTS;
  TDTBGOpts = DTBGOPTS;

// ------------------------------------------------------------------------
//  DrawThemeBackgroundEx()
//                      - draws the theme-specified border and fill for
//                        the "iPartId" and "iStateId".  This could be
//                        based on a bitmap file, a border and fill, or
//                        other image description.  NOTE: This will be
//                        merged back into DrawThemeBackground() after
//                        BETA 2.
//
//  hTheme              - theme data handle
//  hdc                 - HDC to draw into
//  iPartId             - part number to draw
//  iStateId            - state number (of the part) to draw
//  pRect               - defines the size/location of the part
//  pOptions            - ptr to optional params
// ------------------------------------------------------------------------
function DrawThemeBackgroundEx(hTheme: HTHEME; hdc: HDC; iPartId: Integer;
  iStateId: Integer; const pRect: TRect; pOptions: PDTBGOPTS): HResult;

// ---------------------------------------------------------------------------
// 
// DrawThemeTextEx
// 

// Callback function used by DrawTextWithGlow instead of DrawTextW
type
  DTT_CALLBACK_PROC = function(hdc: HDC; pszText: LPWSTR; cchText: Integer;
    prc: PRect; dwFlags: UINT; lParam: LPARAM): Integer; stdcall;
  TFNDTTCallbackProc = DTT_CALLBACK_PROC;


// ---- bits used in dwFlags of DTTOPTS ----
const
  DTT_TEXTCOLOR       = 1 shl 0;        // crText has been specified
  DTT_BORDERCOLOR     = 1 shl 1;        // crBorder has been specified
  DTT_SHADOWCOLOR     = 1 shl 2;        // crShadow has been specified
  DTT_SHADOWTYPE      = 1 shl 3;        // iTextShadowType has been specified
  DTT_SHADOWOFFSET    = 1 shl 4;        // ptShadowOffset has been specified
  DTT_BORDERSIZE      = 1 shl 5;        // iBorderSize has been specified
  DTT_FONTPROP        = 1 shl 6;        // iFontPropId has been specified
  DTT_COLORPROP       = 1 shl 7;        // iColorPropId has been specified
  DTT_STATEID         = 1 shl 8;        // IStateId has been specified
  DTT_CALCRECT        = 1 shl 9;        // Use pRect as and in/out parameter
  DTT_APPLYOVERLAY    = 1 shl 10;       // fApplyOverlay has been specified
  DTT_GLOWSIZE        = 1 shl 11;       // iGlowSize has been specified
  DTT_CALLBACK        = 1 shl 12;       // pfnDrawTextCallback has been specified
  DTT_COMPOSITED      = 1 shl 13;       // Draws text with antialiased alpha (needs a DIB section)
  DTT_VALIDBITS       = DTT_TEXTCOLOR or
                        DTT_BORDERCOLOR or
                        DTT_SHADOWCOLOR or
                        DTT_SHADOWTYPE or
                        DTT_SHADOWOFFSET or
                        DTT_BORDERSIZE or
                        DTT_FONTPROP or
                        DTT_COLORPROP or
                        DTT_STATEID or
                        DTT_CALCRECT or
                        DTT_APPLYOVERLAY or
                        DTT_GLOWSIZE or
                        DTT_COMPOSITED;

type
  PDTTOPTS = ^DTTOPTS;
  DTTOPTS = packed record
    dwSize: DWORD;                          // size of the struct
    dwFlags: DWORD;                         // which options have been specified
    crText: COLORREF;                       // color to use for text fill
    crBorder: COLORREF;                     // color to use for text outline
    crShadow: COLORREF;                     // color to use for text shadow
    iTextShadowType: Integer;               // TST_SINGLE or TST_CONTINUOUS
    ptShadowOffset: TPoint;                 // where shadow is drawn (relative to text)
    iBorderSize: Integer;                   // Border radius around text
    iFontPropId: Integer;                   // Font property to use for the text instead of TMT_FONT
    iColorPropId: Integer;                  // Color property to use for the text instead of TMT_TEXTCOLOR
    iStateId: Integer;                      // Alternate state id
    fApplyOverlay: BOOL;                    // Overlay text on top of any text effect?
    iGlowSize: Integer;                     // Glow radious around text
    pfnDrawTextCallback: TFNDTTCallbackProc;// Callback for DrawText
    lParam: LPARAM;                         // Parameter for callback
  end;
  _DTTOPTS = DTTOPTS;
  TDTTOpts = DTTOPTS;

function DrawThemeTextEx(hTheme: HTHEME; hdc: HDC; iPartId: Integer;
  iStateId: Integer; pszText: LPCWSTR; cchText: Integer; dwTextFlags: DWORD;
  pRect: PRect; var pOptions: TDTTOpts): HResult; overload;
function DrawThemeTextEx(hTheme: HTHEME; hdc: HDC; iPartId: Integer;
  iStateId: Integer; pszText: PWideChar; cchText: Integer; dwTextFlags: DWORD;
  var pRect: TRect; var pOptions: TDTTOpts): HResult; overload; {inline;}

const
  WTA_NONCLIENT   = 1;

type
  PWTA_OPTIONS = ^WTA_OPTIONS;
  WTA_OPTIONS = packed record
    dwFlags: DWORD;         // values for each style option specified in the bitmask
    dwMask: DWORD;          // bitmask for flags that are changing
                            // valid options are: WTNCA_NODRAWCAPTION, WTNCA_NODRAWICON, WTNCA_NOSYSMENU
  end;
  _WTA_OPTIONS = WTA_OPTIONS;
  TWTAOptions = WTA_OPTIONS;
  PWTAOptions = ^TWTAOptions;

const
  WTNCA_NODRAWCAPTION       = $00000001;    // don't draw the window caption
  WTNCA_NODRAWICON          = $00000002;    // don't draw the system icon
  WTNCA_NOSYSMENU           = $00000004;    // don't expose the system menu icon functionality
  WTNCA_NOMIRRORHELP        = $00000008;    // don't mirror the question mark, even in RTL layout
  WTNCA_VALIDBITS           = WTNCA_NODRAWCAPTION or
                              WTNCA_NODRAWICON or
                              WTNCA_NOSYSMENU or
                              WTNCA_NOMIRRORHELP;

function SetWindowThemeAttribute(hwnd: HWND; eAttribute: Cardinal;
  pvAttribute: Pointer; cbAttribute: DWORD): HResult;

function SetWindowThemeNonClientAttributes(hwnd: HWND; dwMask: DWORD;
  dwAttributes: DWORD): HResult;

const
  DTPB_WINDOWDC           = $00000001;
  DTPB_USECTLCOLORSTATIC  = $00000002;
  DTPB_USEERASEBKGND      = $00000004;

// ---------------------------------------------------------------------------
// DrawThemeParentBackgroundEx()
//                      - used by partially-transparent or alpha-blended
//                        child controls to draw the part of their parent
//                        that they appear in front of.
//                        Sends a WM_ERASEBKGND message followed by a WM_PRINTCLIENT.
//
//  hwnd                - handle of the child control
//
//  hdc                 - hdc of the child control
//
//  dwFlags             - if 0, only returns S_OK if the parent handled
//                        WM_PRINTCLIENT.
//                      - if DTPB_WINDOWDC is set, hdc is assumed to be a window DC,
//                        not a client DC.
//                      - if DTPB_USEERASEBKGND is set, the function will return S_OK
//                        without sending a WM_CTLCOLORSTATIC message if the parent
//                        actually painted on WM_ERASEBKGND.
//                      - if DTPB_CTLCOLORSTATIC is set, the function will send
//                        a WM_CTLCOLORSTATIC message to the parent and use the
//                        brush if one is provided, else COLOR_BTNFACE.
//
//  prc                 - (optional) rect that defines the area to be
//                        drawn (CHILD coordinates)
//
//  Return value        - S_OK if something was painted, S_FALSE if not.
// ---------------------------------------------------------------------------
function DrawThemeParentBackgroundEx(hwnd: HWND; hdc: HDC; dwFlags: DWORD;
  prc: PRect): HResult;

const
  GBF_DIRECT      = $00000001;      // direct dereferencing.
  GBF_COPY        = $00000002;      // create a copy of the bitmap
  GBF_VALIDBITS   = GBF_DIRECT or GBF_COPY;

function GetThemeBitmap(hTheme: HTHEME; iPartId: Integer; iStateId: Integer;
  iPropId: Integer; dwFlags: ULONG; var phBitmap: HBITMAP): HResult;

// -----------------------------------------------------------------------
//  GetThemeStream() - Get the value for the specified STREAM property
//
//      hTheme      - theme data handle
//      iPartId     - part number
//      iStateId    - state number of part
//      iPropId     - the property number to get the value for
//      ppvStream   - if non-null receives the value of the STREAM property (not to be freed)
//      pcbStream   - if non-null receives the size of the STREAM property
//      hInst       - NULL when iPropId==TMT_STREAM, HINSTANCE of a loaded msstyles
//                    file when iPropId==TMT_DISKSTREAM (use GetCurrentThemeName
//                    and LoadLibraryEx(LOAD_LIBRARY_AS_DATAFILE)
// -----------------------------------------------------------------------
function GetThemeStream(hTheme: HTHEME; iPartId: Integer; iStateId: Integer;
  iPropId: Integer; var ppvStream: Pointer; var pcbStream: DWORD;
  hInst: HINST): HResult;

// ------------------------------------------------------------------------
//  BufferedPaintInit() - Initialize the Buffered Paint API.
//                        Should be called prior to BeginBufferedPaint,
//                        and should have a matching BufferedPaintUnInit.
// ------------------------------------------------------------------------
function BufferedPaintInit: HResult;

// ------------------------------------------------------------------------
//  BufferedPaintUnInit() - Uninitialize the Buffered Paint API.
//                          Should be called once for each call to BufferedPaintInit,
//                          when calls to BeginBufferedPaint are no longer needed.
// ------------------------------------------------------------------------
function BufferedPaintUnInit: HResult;

// ------------------------------------------------------------------------
//  BeginBufferedPaint() - Begins a buffered paint operation.
//
//    hdcTarget          - Target DC on which the buffer will be painted
//    rcTarget           - Rectangle specifying the area of the target DC to paint to
//    dwFormat           - Format of the buffer (see BP_BUFFERFORMAT)
//    pPaintParams       - Paint operation parameters (see BP_PAINTPARAMS)
//    phBufferedPaint    - Pointer to receive handle to new buffered paint context
// ------------------------------------------------------------------------

type
  // HPAINTBUFFER

  HPAINTBUFFER = THandle;     // handle to a buffered paint context

const
  // BP_BUFFERFORMAT

  BPBF_COMPATIBLEBITMAP = 0;    // Compatible bitmap
  BPBF_DIB = 1;                 // Device-independent bitmap
  BPBF_TOPDOWNDIB = 2;          // Top-down device-independent bitmap
  BPBF_TOPDOWNMONODIB = 3;      // Top-down monochrome device-independent bitmap
  BPBF_COMPOSITED = BPBF_TOPDOWNDIB;


  // BP_ANIMATIONSTYLE

  BPAS_NONE = 0;                // No animation
  BPAS_LINEAR = 1;              // Linear fade animation
  BPAS_CUBIC = 2;               // Cubic fade animation
  BPAS_SINE = 3;                 // Sinusoid fade animation


type
  // BP_ANIMATIONPARAMS

  PBP_ANIMATIONPARAMS = ^BP_ANIMATIONPARAMS;
  BP_ANIMATIONPARAMS = packed record
    cbSize: DWORD;
    dwFlags: DWORD;              // BPAF_ flags
    style: Cardinal;
    dwDuration: DWORD;
  end;
  _BP_ANIMATIONPARAMS = BP_ANIMATIONPARAMS;
  TBPAnimationParams = BP_ANIMATIONPARAMS;
  PBPAnimationParams = ^TBPAnimationParams;

const
  BPPF_ERASE               = $0001; // Empty the buffer during BeginBufferedPaint()
  BPPF_NOCLIP              = $0002; // Don't apply the target DC's clip region to the double buffer
  BPPF_NONCLIENT           = $0004; // Using a non-client DC


type
  // BP_PAINTPARAMS

  PBP_PAINTPARAMS = ^BP_PAINTPARAMS;
  BP_PAINTPARAMS = packed record
    cbSize: DWORD;
    dwFlags: DWORD;                      // BPPF_ flags
    prcExclude: PRect;
    pBlendFunction: PBLENDFUNCTION;
  end;
  _BP_PAINTPARAMS = BP_PAINTPARAMS;
  TBPPaintParams = BP_PAINTPARAMS;
  PBPPaintParams = ^TBPPaintParams;

function BeginBufferedPaint(hdcTarget: HDC; const prcTarget: TRect; 
  dwFormat: DWORD; pPaintParams: PBPPaintParams; var phdc: HDC): HPAINTBUFFER;

// ------------------------------------------------------------------------
//  EndBufferedPaint() - Ends a buffered paint operation.
//
//    hBufferedPaint   - handle to buffered paint context
//    fUpdateTarget    - update target DC
// ------------------------------------------------------------------------
function EndBufferedPaint(hBufferedPaint: HPAINTBUFFER;
  fUpdateTarget: BOOL): HResult;

// ------------------------------------------------------------------------
//  GetBufferedPaintTargetRect() - Returns the target rectangle specified during BeginBufferedPaint
//
//    hBufferedPaint             - handle to buffered paint context
//    prc                        - pointer to receive target rectangle
// ------------------------------------------------------------------------
function GetBufferedPaintTargetRect(hBufferedPaint: HPAINTBUFFER;
  var prc: TRect): HResult;

// ------------------------------------------------------------------------
//  GetBufferedPaintTargetDC() - Returns the target DC specified during BeginBufferedPaint
//
//    hBufferedPaint           - handle to buffered paint context
// ------------------------------------------------------------------------
function GetBufferedPaintTargetDC(hBufferedPaint: HPAINTBUFFER): HDC;

// ------------------------------------------------------------------------
//  GetBufferedPaintDC() - Returns the same paint DC returned by BeginBufferedPaint
//
//    hBufferedPaint     - handle to buffered paint context
// ------------------------------------------------------------------------
function GetBufferedPaintDC(hBufferedPaint: HPAINTBUFFER): HDC;

// ------------------------------------------------------------------------
//  GetBufferedPaintBits() - Obtains a pointer to the buffer bitmap, if the buffer is a DIB
// 
//    hBufferedPaint       - handle to buffered paint context
//    ppbBuffer            - pointer to receive pointer to buffer bitmap pixels
//    pcxRow               - pointer to receive width of buffer bitmap, in pixels;
//                           this value may not necessarily be equal to the buffer width
// ------------------------------------------------------------------------
function GetBufferedPaintBits(hBufferedPaint: HPAINTBUFFER;
  var ppbBuffer: PRGBQUAD; var pcxRow: Integer): HResult;

// ------------------------------------------------------------------------
//  BufferedPaintClear() - Clears given rectangle to ARGB = {0, 0, 0, 0}
//
//    hBufferedPaint     - handle to buffered paint context
//    prc                - rectangle to clear; NULL specifies entire buffer
// ------------------------------------------------------------------------
function BufferedPaintClear(hBufferedPaint: HPAINTBUFFER; prc: PRect): HResult;

// ------------------------------------------------------------------------
//  BufferedPaintSetAlpha() - Set alpha to given value in given rectangle
//
//    hBufferedPaint        - handle to buffered paint context
//    prc                   - rectangle to set alpha in; NULL specifies entire buffer
//    alpha                 - alpha value to set in the given rectangle
// ------------------------------------------------------------------------
function BufferedPaintSetAlpha(hBufferedPaint: HPAINTBUFFER; prc: PRect;
  alpha: Byte): HResult;

// Macro for setting the buffer to opaque (alpha = 255)
function BufferedPaintMakeOpaque(hBufferedPaint: HPAINTBUFFER; prc: PRect): HResult; {inline;} overload;
function BufferedPaintMakeOpaque(hBufferedPaint: HPAINTBUFFER; const prc: TRect): HResult; {inline;} overload;

// ------------------------------------------------------------------------
//  BufferedPaintStopAllAnimations() - Stop all buffer animations for the given window
//
//    hwnd                           - window on which to stop all animations
// ------------------------------------------------------------------------
function BufferedPaintStopAllAnimations(hwnd: HWND): HResult;

type
  HANIMATIONBUFFER = THandle;     // handle to a buffered paint animation

function BeginBufferedAnimation(hwnd: HWND; hdcTarget: HDC;
  var prcTarget: TRect; dwFormat: DWORD; pPaintParams: PBPPaintParams;
  var pAnimationParams: TBPAnimationParams; var phdcFrom: HDC; var phdcTo: HDC): HANIMATIONBUFFER;

function EndBufferedAnimation(hbpAnimation: HANIMATIONBUFFER; 
  fUpdateTarget: BOOL): HResult;

function BufferedPaintRenderAnimation(hwnd: HWND; hdcTarget: HDC): BOOL;

// ----------------------------------------------------------------------------
// Tells if the DWM is running, and composition effects are possible for this
// process (themes are active).
// Roughly equivalent to "DwmIsCompositionEnabled() && IsAppthemed()"
// ----------------------------------------------------------------------------
function IsCompositionActive: BOOL;

// ------------------------------------------------------------------------
//  GetThemeTransitionDuration()
//                      - Gets the duration for the specified transition
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateIdFrom        - starting state number of part
//  iStateIdTo          - ending state number of part
//  iPropId             - property id
//  pdwDuration         - receives the transition duration
// ------------------------------------------------------------------------
function GetThemeTransitionDuration(hTheme: HTHEME; iPartId: Integer;
  iStateIdFrom: Integer; iStateIdTo: Integer; iPropId: Integer;
  var pdwDuration: DWORD): HResult;


implementation

{uses
  SyncObjs;}

const
  themelib = 'uxtheme.dll';

var
  ThemeLibrary: THandle;
  ReferenceCount: Integer;
//  Lock: TCriticalSection;

var
  _OpenThemeDataEx: function(hwnd: HWND; pszClassList: LPCWSTR; dwFlags: DWORD): HTHEME; stdcall;

  _DrawThemeBackgroundEx: function(hTheme: HTHEME; hdc: HDC; iPartId: Integer;
    iStateId: Integer; const pRect: TRect; pOptions: PDTBGOPTS): HResult; stdcall;

  _DrawThemeTextEx: function(hTheme: HTHEME; hdc: HDC; iPartId: Integer;
    iStateId: Integer; pszText: LPCWSTR; cchText: Integer;
    dwTextFlags: DWORD; pRect: PRect; var pOptions: TDTTOpts): HResult; stdcall;

  _DrawThemeParentBackgroundEx: function(hwnd: HWND; hdc: HDC; dwFlags: DWORD;
    prc: PRect): HResult; stdcall;

  _SetWindowThemeAttribute: function(hwnd: HWND; eAttribute: Cardinal;
    pvAttribute: Pointer; cbAttribute: DWORD): HResult; stdcall;

  _GetThemeBitmap: function(hTheme: HTHEME; iPartId: Integer; iStateId: Integer;
    iPropId: Integer; dwFlags: ULONG; var phBitmap: HBITMAP): HResult; stdcall;

  _GetThemeStream: function(hTheme: HTHEME; iPartId: Integer; iStateId: Integer;
    iPropId: Integer; var ppvStream: Pointer; var pcbStream: DWORD;
    hInst: HINST): HResult; stdcall;

  _BufferedPaintInit: function: HResult; stdcall;

  _BufferedPaintUnInit: function: HResult; stdcall;

  _BeginBufferedPaint: function(hdcTarget: HDC; const prcTarget: TRect; 
    dwFormat: DWORD; pPaintParams: PBPPaintParams; var phdc: HDC): HPAINTBUFFER; stdcall;

  _EndBufferedPaint: function(hBufferedPaint: HPAINTBUFFER; fUpdateTarget: BOOL): HResult; stdcall;

  _GetBufferedPaintTargetRect: function(hBufferedPaint: HPAINTBUFFER; 
    var prc: TRect): HResult; stdcall;

  _GetBufferedPaintTargetDC: function(hBufferedPaint: HPAINTBUFFER): HDC; stdcall;

  _GetBufferedPaintDC: function(hBufferedPaint: HPAINTBUFFER): HDC; stdcall;

  _GetBufferedPaintBits: function(hBufferedPaint: HPAINTBUFFER; 
    var ppbBuffer: PRGBQUAD; var pcxRow: Integer): HResult; stdcall;

  _BufferedPaintClear: function(hBufferedPaint: HPAINTBUFFER; prc: PRect): HResult; stdcall;

  _BufferedPaintSetAlpha: function(hBufferedPaint: HPAINTBUFFER; prc: PRect;
    alpha: Byte): HResult; stdcall;

  _BufferedPaintStopAllAnimations: function(hwnd: HWND): HResult; stdcall;

  _BeginBufferedAnimation: function(hwnd: HWND; hdcTarget: HDC; 
    var prcTarget: TRect; dwFormat: DWORD; pPaintParams: PBPPaintParams;
    var pAnimationParams: TBPAnimationParams; var phdcFrom: HDC;
    var phdcTo: HDC): HANIMATIONBUFFER; stdcall;

  _EndBufferedAnimation: function(hbpAnimation: HANIMATIONBUFFER;
    fUpdateTarget: BOOL): HResult; stdcall;

  _BufferedPaintRenderAnimation: function(hwnd: HWND; hdcTarget: HDC): BOOL; stdcall;

  _IsCompositionActive: function: BOOL; stdcall;

  _GetThemeTransitionDuration: function(hTheme: HTHEME; iPartId: Integer;
    iStateIdFrom: Integer; iStateIdTo: Integer; iPropId: Integer;
    var pdwDuration: DWORD): HResult; stdcall;
    

function OpenThemeDataEx(hwnd: HWND; pszClassList: LPCWSTR; dwFlags: DWORD): HTHEME;
begin
  if Assigned(_OpenThemeDataEx) then
    Result := _OpenThemeDataEx(hwnd, pszClassList, dwFlags)
  else
  begin
    Result := 0;
    if ThemeLibrary > 0 then
    begin
      _OpenThemeDataEx := GetProcAddress(ThemeLibrary, PAnsiChar('OpenThemeDataEx')); // Do not localize
      if Assigned(_OpenThemeDataEx) then
        Result := _OpenThemeDataEx(hwnd, pszClassList, dwFlags);
    end;
  end;
end;

function DrawThemeBackgroundEx(hTheme: HTHEME; hdc: HDC; iPartId: Integer;
  iStateId: Integer; const pRect: TRect; pOptions: PDTBGOPTS): HResult;
begin
  if Assigned(_DrawThemeBackgroundEx) then
    Result := _DrawThemeBackgroundEx(hTheme, hdc, iPartId, iStateId, pRect,
      pOptions)
  else
  begin
    Result := E_NOTIMPL;
    if ThemeLibrary > 0 then
    begin
      _DrawThemeBackgroundEx := GetProcAddress(ThemeLibrary, PAnsiChar('DrawThemeBackgroundEx')); // Do not localize
      if Assigned(_DrawThemeBackgroundEx) then
        Result := _DrawThemeBackgroundEx(hTheme, hdc, iPartId, iStateId, pRect,
          pOptions);
    end;
  end;
end;

function SetWindowThemeAttribute(hwnd: HWND; eAttribute: Cardinal;
  pvAttribute: Pointer; cbAttribute: DWORD): HResult;
begin
  if Assigned(_SetWindowThemeAttribute) then
    Result := _SetWindowThemeAttribute(hwnd, eAttribute, pvAttribute, cbAttribute)
  else
  begin
    Result := E_NOTIMPL;
    if ThemeLibrary > 0 then
    begin
      _SetWindowThemeAttribute := GetProcAddress(ThemeLibrary, PAnsiChar('SetWindowThemeAttribute')); // Do not localize
      if Assigned(_SetWindowThemeAttribute) then
        Result := _SetWindowThemeAttribute(hwnd, eAttribute, pvAttribute, cbAttribute);
    end;
  end;
end;

function SetWindowThemeNonClientAttributes(hwnd: HWND; dwMask: DWORD;
  dwAttributes: DWORD): HResult;
var
  WTA: TWTAOptions;
begin
  WTA.dwFlags := dwAttributes;
  WTA.dwMask := dwMask;
  Result := SetWindowThemeAttribute(hwnd, WTA_NONCLIENT, @WTA, SizeOf(WTA));
end;

function DrawThemeTextEx(hTheme: HTHEME; hdc: HDC; iPartId: Integer;
  iStateId: Integer; pszText: LPCWSTR; cchText: Integer; dwTextFlags: DWORD;
  pRect: PRect; var pOptions: TDTTOpts): HResult;
begin
  if Assigned(_DrawThemeTextEx) then
    Result := _DrawThemeTextEx(hTheme, hdc, iPartId, iStateId, pszText, cchText,
      dwTextFlags, pRect, pOptions)
  else
  begin
    Result := E_NOTIMPL;
    if ThemeLibrary > 0 then
    begin
      _DrawThemeTextEx := GetProcAddress(ThemeLibrary, PAnsiChar('DrawThemeTextEx')); // Do not localize
      if Assigned(_DrawThemeTextEx) then
        Result := _DrawThemeTextEx(hTheme, hdc, iPartId, iStateId, pszText,
          cchText, dwTextFlags, pRect, pOptions);
    end;
  end;
end;

function DrawThemeTextEx(hTheme: HTHEME; hdc: HDC; iPartId: Integer;
  iStateId: Integer; pszText: PWideChar; cchText: Integer; dwTextFlags: DWORD;
  var pRect: TRect; var pOptions: TDTTOpts): HResult;
begin
  Result := DrawThemeTextEx(hTheme, hdc, iPartId, iStateId,
    PWideChar(pszText), cchText, dwTextFlags, @pRect, pOptions);
end;

function DrawThemeParentBackgroundEx(hwnd: HWND; hdc: HDC; dwFlags: DWORD;
  prc: PRect): HResult;
begin
  if Assigned(_DrawThemeParentBackgroundEx) then
    Result := _DrawThemeParentBackgroundEx(hwnd, hdc, dwFlags, prc)
  else
  begin
    Result := E_NOTIMPL;
    if ThemeLibrary > 0 then
    begin
      _DrawThemeParentBackgroundEx := GetProcAddress(ThemeLibrary, PAnsiChar('DrawThemeParentBackgroundEx')); // Do not localize
      if Assigned(_DrawThemeParentBackgroundEx) then
        Result := _DrawThemeParentBackgroundEx(hwnd, hdc, dwFlags, prc);
    end;
  end;
end;

function GetThemeBitmap(hTheme: HTHEME; iPartId: Integer; iStateId: Integer;
  iPropId: Integer; dwFlags: ULONG; var phBitmap: HBITMAP): HResult;
begin
  if Assigned(_GetThemeBitmap) then
    Result := _GetThemeBitmap(hTheme, iPartId, iStateId, iPropId, dwFlags,
      phBitmap)
  else
  begin
    Result := E_NOTIMPL;
    if ThemeLibrary > 0 then
    begin
      _GetThemeBitmap := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemeBitmap')); // Do not localize
      if Assigned(_GetThemeBitmap) then
        Result := _GetThemeBitmap(hTheme, iPartId, iStateId, iPropId, dwFlags,
          phBitmap);
    end;
  end;
end;

function GetThemeStream(hTheme: HTHEME; iPartId: Integer; iStateId: Integer;
  iPropId: Integer; var ppvStream: Pointer; var pcbStream: DWORD;
  hInst: HINST): HResult;
begin
  if Assigned(_GetThemeStream) then
    Result := _GetThemeStream(hTheme, iPartId, iStateId, iPropId, ppvStream,
      pcbStream, hInst)
  else
  begin
    Result := E_NOTIMPL;
    if ThemeLibrary > 0 then
    begin
      _GetThemeStream := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemeStream')); // Do not localize
      if Assigned(_GetThemeStream) then
        Result := _GetThemeStream(hTheme, iPartId, iStateId, iPropId, ppvStream,
          pcbStream, hInst);
    end;
  end;
end;

function BufferedPaintInit: HResult;
begin
  if Assigned(_BufferedPaintInit) then
    Result := _BufferedPaintInit
  else
  begin
    Result := E_NOTIMPL;
    if ThemeLibrary > 0 then
    begin
      _BufferedPaintInit := GetProcAddress(ThemeLibrary, PAnsiChar('BufferedPaintInit')); // Do not localize
      if Assigned(_BufferedPaintInit) then
        Result := _BufferedPaintInit;
    end;
  end;
end;

function BufferedPaintUnInit: HResult;
begin
  if Assigned(_BufferedPaintUnInit) then
    Result := _BufferedPaintUnInit
  else
  begin
    Result := E_NOTIMPL;
    if ThemeLibrary > 0 then
    begin
      _BufferedPaintUnInit := GetProcAddress(ThemeLibrary, PAnsiChar('BufferedPaintUnInit')); // Do not localize
      if Assigned(_BufferedPaintUnInit) then
        Result := _BufferedPaintUnInit;
    end;
  end;
end;

function BeginBufferedPaint(hdcTarget: HDC; const prcTarget: TRect;
  dwFormat: DWORD; pPaintParams: PBPPaintParams; var phdc: HDC): HPAINTBUFFER;
begin
  if Assigned(_BeginBufferedPaint) then
    Result := _BeginBufferedPaint(hdcTarget, prcTarget, dwFormat, pPaintParams,
      phdc)
  else
  begin
    Result := 0;
    if ThemeLibrary > 0 then
    begin
      _BeginBufferedPaint := GetProcAddress(ThemeLibrary, PAnsiChar('BeginBufferedPaint')); // Do not localize
      if Assigned(_BeginBufferedPaint) then
        Result := _BeginBufferedPaint(hdcTarget, prcTarget, dwFormat,
          pPaintParams, phdc);
    end;
  end;
end;

function EndBufferedPaint(hBufferedPaint: HPAINTBUFFER; fUpdateTarget: BOOL): HResult;
begin
  if Assigned(_EndBufferedPaint) then
    Result := _EndBufferedPaint(hBufferedPaint, fUpdateTarget)
  else
  begin
    Result := E_NOTIMPL;
    if ThemeLibrary > 0 then
    begin
      _EndBufferedPaint := GetProcAddress(ThemeLibrary, PAnsiChar('EndBufferedPaint')); // Do not localize
      if Assigned(_EndBufferedPaint) then
        Result := _EndBufferedPaint(hBufferedPaint, fUpdateTarget);
    end;
  end;
end;

function GetBufferedPaintTargetRect(hBufferedPaint: HPAINTBUFFER; 
  var prc: TRect): HResult;
begin
  if Assigned(_GetBufferedPaintTargetRect) then
    Result := _GetBufferedPaintTargetRect(hBufferedPaint, prc)
  else
  begin
    Result := E_NOTIMPL;
    if ThemeLibrary > 0 then
    begin
      _GetBufferedPaintTargetRect := GetProcAddress(ThemeLibrary, PAnsiChar('GetBufferedPaintTargetRect')); // Do not localize
      if Assigned(_GetBufferedPaintTargetRect) then
        Result := _GetBufferedPaintTargetRect(hBufferedPaint, prc);
    end;
  end;
end;

function GetBufferedPaintTargetDC(hBufferedPaint: HPAINTBUFFER): HDC;
begin
  if Assigned(_GetBufferedPaintTargetDC) then
    Result := _GetBufferedPaintTargetDC(hBufferedPaint)
  else
  begin
    Result := 0;
    if ThemeLibrary > 0 then
    begin
      _GetBufferedPaintTargetDC := GetProcAddress(ThemeLibrary, PAnsiChar('GetBufferedPaintTargetDC')); // Do not localize
      if Assigned(_GetBufferedPaintTargetDC) then
        Result := _GetBufferedPaintTargetDC(hBufferedPaint);
    end;
  end;
end;

function GetBufferedPaintDC(hBufferedPaint: HPAINTBUFFER): HDC;
begin
  if Assigned(_GetBufferedPaintDC) then
    Result := _GetBufferedPaintDC(hBufferedPaint)
  else
  begin
    Result := 0;
    if ThemeLibrary > 0 then
    begin
      _GetBufferedPaintDC := GetProcAddress(ThemeLibrary, PAnsiChar('GetBufferedPaintDC')); // Do not localize
      if Assigned(_GetBufferedPaintDC) then
        Result := _GetBufferedPaintDC(hBufferedPaint);
    end;
  end;
end;

function GetBufferedPaintBits(hBufferedPaint: HPAINTBUFFER; 
  var ppbBuffer: PRGBQUAD; var pcxRow: Integer): HResult;
begin
  if Assigned(_GetBufferedPaintBits) then
    Result := _GetBufferedPaintBits(hBufferedPaint, ppbBuffer, pcxRow)
  else
  begin
    Result := E_NOTIMPL;
    if ThemeLibrary > 0 then
    begin
      _GetBufferedPaintBits := GetProcAddress(ThemeLibrary, PAnsiChar('GetBufferedPaintBits')); // Do not localize
      if Assigned(_GetBufferedPaintBits) then
        Result := _GetBufferedPaintBits(hBufferedPaint, ppbBuffer, pcxRow);
    end;
  end;
end;

function BufferedPaintClear(hBufferedPaint: HPAINTBUFFER; prc: PRect): HResult;
begin
  if Assigned(_BufferedPaintClear) then
    Result := _BufferedPaintClear(hBufferedPaint, prc)
  else
  begin
    Result := E_NOTIMPL;
    if ThemeLibrary > 0 then
    begin
      _BufferedPaintClear := GetProcAddress(ThemeLibrary, PAnsiChar('BufferedPaintClear')); // Do not localize
      if Assigned(_BufferedPaintClear) then
        Result := _BufferedPaintClear(hBufferedPaint, prc);
    end;
  end;
end;

function BufferedPaintSetAlpha(hBufferedPaint: HPAINTBUFFER; prc: PRect; 
  alpha: Byte): HResult;
begin
  if Assigned(_BufferedPaintSetAlpha) then
    Result := _BufferedPaintSetAlpha(hBufferedPaint, prc, alpha)
  else
  begin
    Result := E_NOTIMPL;
    if ThemeLibrary > 0 then
    begin
      _BufferedPaintSetAlpha := GetProcAddress(ThemeLibrary, PAnsiChar('BufferedPaintSetAlpha')); // Do not localize
      if Assigned(_BufferedPaintSetAlpha) then
        Result := _BufferedPaintSetAlpha(hBufferedPaint, prc, alpha);
    end;
  end;
end;

function BufferedPaintMakeOpaque(hBufferedPaint: HPAINTBUFFER; prc: PRect): HResult;
begin
  Result := BufferedPaintSetAlpha(hBufferedPaint, prc, 255);
end;

function BufferedPaintMakeOpaque(hBufferedPaint: HPAINTBUFFER; const prc: TRect): HResult;
begin
  Result := BufferedPaintSetAlpha(hBufferedPaint, @prc, 255);
end;

function BufferedPaintStopAllAnimations(hwnd: HWND): HResult;
begin
  if Assigned(_BufferedPaintStopAllAnimations) then
    Result := _BufferedPaintStopAllAnimations(hwnd)
  else
  begin
    Result := E_NOTIMPL;
    if ThemeLibrary > 0 then
    begin
      _BufferedPaintStopAllAnimations := GetProcAddress(ThemeLibrary, PAnsiChar('BufferedPaintStopAllAnimations')); // Do not localize
      if Assigned(_BufferedPaintStopAllAnimations) then
        Result := _BufferedPaintStopAllAnimations(hwnd);
    end;
  end;
end;

function BeginBufferedAnimation(hwnd: HWND; hdcTarget: HDC; 
  var prcTarget: TRect; dwFormat: DWORD; pPaintParams: PBPPaintParams;
  var pAnimationParams: TBPAnimationParams; var phdcFrom: HDC;
  var phdcTo: HDC): HANIMATIONBUFFER;
begin
  if Assigned(_BeginBufferedAnimation) then
    Result := _BeginBufferedAnimation(hwnd, hdcTarget, prcTarget, dwFormat, 
      pPaintParams, pAnimationParams, phdcFrom, phdcTo)
  else
  begin
    Result := 0;
    if ThemeLibrary > 0 then
    begin
      _BeginBufferedAnimation := GetProcAddress(ThemeLibrary, PAnsiChar('BeginBufferedAnimation')); // Do not localize
      if Assigned(_BeginBufferedAnimation) then
        Result := _BeginBufferedAnimation(hwnd, hdcTarget, prcTarget, dwFormat, 
          pPaintParams, pAnimationParams, phdcFrom, phdcTo);
    end;
  end;
end;

function EndBufferedAnimation(hbpAnimation: HANIMATIONBUFFER; 
  fUpdateTarget: BOOL): HResult;
begin
  if Assigned(_EndBufferedAnimation) then
    Result := _EndBufferedAnimation(hbpAnimation, fUpdateTarget)
  else
  begin
    Result := E_NOTIMPL;
    if ThemeLibrary > 0 then
    begin
      _EndBufferedAnimation := GetProcAddress(ThemeLibrary, PAnsiChar('EndBufferedAnimation')); // Do not localize
      if Assigned(_EndBufferedAnimation) then
        Result := _EndBufferedAnimation(hbpAnimation, fUpdateTarget);
    end;
  end;
end;

function BufferedPaintRenderAnimation(hwnd: HWND; hdcTarget: HDC): BOOL;
begin
  if Assigned(_BufferedPaintRenderAnimation) then
    Result := _BufferedPaintRenderAnimation(hwnd, hdcTarget)
  else
  begin
    Result := False;
    if ThemeLibrary > 0 then
    begin
      _BufferedPaintRenderAnimation := GetProcAddress(ThemeLibrary, PAnsiChar('BufferedPaintRenderAnimation')); // Do not localize
      if Assigned(_BufferedPaintRenderAnimation) then
        Result := _BufferedPaintRenderAnimation(hwnd, hdcTarget);
    end;
  end;
end;

function IsCompositionActive: BOOL;
begin
  if Assigned(_IsCompositionActive) then
    Result := _IsCompositionActive
  else
  begin
    Result := False;
    if ThemeLibrary > 0 then
    begin
      _IsCompositionActive := GetProcAddress(ThemeLibrary, PAnsiChar('IsCompositionActive')); // Do not localize
      if Assigned(_IsCompositionActive) then
        Result := _IsCompositionActive;
    end;
  end;
end;

function GetThemeTransitionDuration(hTheme: HTHEME; iPartId: Integer;
  iStateIdFrom: Integer; iStateIdTo: Integer; iPropId: Integer;
  var pdwDuration: DWORD): HResult;
begin
  if Assigned(_GetThemeTransitionDuration) then
    Result := _GetThemeTransitionDuration(hTheme, iPartId, iStateIdFrom,
      iStateIdTo, iPropId, pdwDuration)
  else
  begin
    Result := E_NOTIMPL;
    if ThemeLibrary > 0 then
    begin
      _GetThemeTransitionDuration := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemeTransitionDuration')); // Do not localize
      if Assigned(_GetThemeTransitionDuration) then
        Result := _GetThemeTransitionDuration(hTheme, iPartId, iStateIdFrom,
          iStateIdTo, iPropId, pdwDuration);
    end;
  end;
end;

procedure FreeThemeLibrary;
begin
//  Lock.Enter;
  try
    if ReferenceCount > 0 then
      Dec(ReferenceCount);

    if (ThemeLibrary <> 0) and (ReferenceCount = 0) then
    begin
      FreeLibrary(ThemeLibrary);
      ThemeLibrary := 0;

      OpenThemeData := nil;
      CloseThemeData := nil;
      DrawThemeBackground := nil;
      DrawThemeText := nil;
      GetThemeBackgroundContentRect := nil;
      GetThemeBackgroundExtent := nil;
      GetThemePartSize := nil;
      GetThemeTextExtent := nil;
      GetThemeTextMetrics := nil;
      GetThemeBackgroundRegion := nil;
      HitTestThemeBackground := nil;
      DrawThemeEdge := nil;
      DrawThemeIcon := nil;
      IsThemePartDefined := nil;
      IsThemeBackgroundPartiallyTransparent := nil;
      GetThemeColor := nil;
      GetThemeMetric := nil;
      GetThemeString := nil;
      GetThemeBool := nil;
      GetThemeInt := nil;
      GetThemeEnumValue := nil;
      GetThemePosition := nil;
      GetThemeFont := nil;
      GetThemeRect := nil;
      GetThemeMargins := nil;
      GetThemeIntList := nil;
      GetThemePropertyOrigin := nil;
      SetWindowTheme := nil;
      GetThemeFilename := nil;
      GetThemeSysColor := nil;
      GetThemeSysColorBrush := nil;
      GetThemeSysBool := nil;
      GetThemeSysSize := nil;
      GetThemeSysFont := nil;
      GetThemeSysString := nil;
      GetThemeSysInt := nil;
      IsThemeActive := nil;
      IsAppThemed := nil;
      GetWindowTheme := nil;
      EnableThemeDialogTexture := nil;
      IsThemeDialogTextureEnabled := nil;
      GetThemeAppProperties := nil;
      SetThemeAppProperties := nil;
      GetCurrentThemeName := nil;
      GetThemeDocumentationProperty := nil;
      DrawThemeParentBackground := nil;
      EnableTheming := nil;
    end;
  finally
//    Lock.Leave;
  end;
end;

function InitThemeLibrary: Boolean;
begin
//  Lock.Enter;
  try
    Inc(ReferenceCount);

    if ThemeLibrary = 0 then
    begin
      ThemeLibrary := LoadLibrary(themelib);
      if ThemeLibrary > 0 then
      begin
        OpenThemeData := GetProcAddress(ThemeLibrary, PAnsiChar('OpenThemeData'));
        CloseThemeData := GetProcAddress(ThemeLibrary, PAnsiChar('CloseThemeData'));
        DrawThemeBackground := GetProcAddress(ThemeLibrary, PAnsiChar('DrawThemeBackground'));
        DrawThemeText := GetProcAddress(ThemeLibrary, PAnsiChar('DrawThemeText'));
        GetThemeBackgroundContentRect := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemeBackgroundContentRect'));
        GetThemeBackgroundExtent := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemeBackgroundExtent'));
        GetThemePartSize := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemePartSize'));
        GetThemeTextExtent := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemeTextExtent'));
        GetThemeTextMetrics := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemeTextMetrics'));
        GetThemeBackgroundRegion := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemeBackgroundRegion'));
        HitTestThemeBackground := GetProcAddress(ThemeLibrary, PAnsiChar('HitTestThemeBackground'));
        DrawThemeEdge := GetProcAddress(ThemeLibrary, PAnsiChar('DrawThemeEdge'));
        DrawThemeIcon := GetProcAddress(ThemeLibrary, PAnsiChar('DrawThemeIcon'));
        IsThemePartDefined := GetProcAddress(ThemeLibrary, PAnsiChar('IsThemePartDefined'));
        IsThemeBackgroundPartiallyTransparent := GetProcAddress(ThemeLibrary, PAnsiChar('IsThemeBackgroundPartiallyTransparent'));
        GetThemeColor := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemeColor'));
        GetThemeMetric := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemeMetric'));
        GetThemeString := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemeString'));
        GetThemeBool := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemeBool'));
        GetThemeInt := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemeInt'));
        GetThemeEnumValue := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemeEnumValue'));
        GetThemePosition := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemePosition'));
        GetThemeFont := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemeFont'));
        GetThemeRect := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemeRect'));
        GetThemeMargins := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemeMargins'));
        GetThemeIntList := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemeIntList'));
        GetThemePropertyOrigin := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemePropertyOrigin'));
        SetWindowTheme := GetProcAddress(ThemeLibrary, PAnsiChar('SetWindowTheme'));
        GetThemeFilename := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemeFilename'));
        GetThemeSysColor := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemeSysColor'));
        GetThemeSysColorBrush := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemeSysColorBrush'));
        GetThemeSysBool := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemeSysBool'));
        GetThemeSysSize := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemeSysSize'));
        GetThemeSysFont := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemeSysFont'));
        GetThemeSysString := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemeSysString'));
        GetThemeSysInt := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemeSysInt'));
        IsThemeActive := GetProcAddress(ThemeLibrary, PAnsiChar('IsThemeActive'));
        IsAppThemed := GetProcAddress(ThemeLibrary, PAnsiChar('IsAppThemed'));
        GetWindowTheme := GetProcAddress(ThemeLibrary, PAnsiChar('GetWindowTheme'));
        EnableThemeDialogTexture := GetProcAddress(ThemeLibrary, PAnsiChar('EnableThemeDialogTexture'));
        IsThemeDialogTextureEnabled := GetProcAddress(ThemeLibrary, PAnsiChar('IsThemeDialogTextureEnabled'));
        GetThemeAppProperties := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemeAppProperties'));
        SetThemeAppProperties := GetProcAddress(ThemeLibrary, PAnsiChar('SetThemeAppProperties'));
        GetCurrentThemeName := GetProcAddress(ThemeLibrary, PAnsiChar('GetCurrentThemeName'));
        GetThemeDocumentationProperty := GetProcAddress(ThemeLibrary, PAnsiChar('GetThemeDocumentationProperty'));
        DrawThemeParentBackground := GetProcAddress(ThemeLibrary, PAnsiChar('DrawThemeParentBackground'));
        EnableTheming := GetProcAddress(ThemeLibrary, PAnsiChar('EnableTheming'));
      end;
    end;
    Result := ThemeLibrary > 0;
  finally
//    Lock.Leave;
  end;
end;

function UseThemes: Boolean;
begin
  if (ThemeLibrary > 0) then
    Result := IsAppThemed and IsThemeActive
  else
    Result := False;
end;

initialization
//  Lock := TCriticalSection.Create;
finalization
  if not IsLibrary then begin
    while ReferenceCount > 0 do
      FreeThemeLibrary;
  //  Lock.Free;
  end;
end.

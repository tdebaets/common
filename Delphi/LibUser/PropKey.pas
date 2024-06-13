unit PropKey;

interface

uses PropSys;

//===========================================================================
//
// Copyright (c) Microsoft Corporation. All rights reserved.
//
//===========================================================================

const
 
//-----------------------------------------------------------------------------
// Audio properties

//  Name:     System.Audio.ChannelCount -- PKEY_Audio_ChannelCount
//  Type:     UInt32 -- VT_UI4
//  FormatID: (FMTID_AudioSummaryInformation) 64440490-4C8B-11D1-8B70-080036B11A03, 7 (PIDASI_CHANNEL_COUNT)
//
//  Indicates the channel count for the audio file.  Values: 1 (mono), 2 (stereo).
  PKEY_Audio_ChannelCount: TPropertyKey = (
      fmtid: (D1: $64440490; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 7);

// Possible discrete values for PKEY_Audio_ChannelCount are:
  AUDIO_CHANNELCOUNT_MONO = 1;
  AUDIO_CHANNELCOUNT_STEREO = 2;

//  Name:     System.Audio.Compression -- PKEY_Audio_Compression
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_AudioSummaryInformation) 64440490-4C8B-11D1-8B70-080036B11A03, 10 (PIDASI_COMPRESSION)
//
//  
  PKEY_Audio_Compression: TPropertyKey = (
      fmtid: (D1: $64440490; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 10);

//  Name:     System.Audio.EncodingBitrate -- PKEY_Audio_EncodingBitrate
//  Type:     UInt32 -- VT_UI4
//  FormatID: (FMTID_AudioSummaryInformation) 64440490-4C8B-11D1-8B70-080036B11A03, 4 (PIDASI_AVG_DATA_RATE)
//
//  Indicates the average data rate in Hz for the audio file in "bits per second".
  PKEY_Audio_EncodingBitrate: TPropertyKey = (
      fmtid: (D1: $64440490; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 4);

//  Name:     System.Audio.Format -- PKEY_Audio_Format
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)  Legacy code may treat this as VT_BSTR.
//  FormatID: (FMTID_AudioSummaryInformation) 64440490-4C8B-11D1-8B70-080036B11A03, 2 (PIDASI_FORMAT)
//
//  Indicates the format of the audio file.
  PKEY_Audio_Format: TPropertyKey = (
      fmtid: (D1: $64440490; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 2);

//  Name:     System.Audio.IsVariableBitRate -- PKEY_Audio_IsVariableBitRate
//  Type:     Boolean -- VT_BOOL
//  FormatID: E6822FEE-8C17-4D62-823C-8E9CFCBD1D5C, 100
  PKEY_Audio_IsVariableBitRate: TPropertyKey = (
      fmtid: (D1: $E6822FEE; D2: $8C17; D3: $4D62;
      D4: ($82, $3C, $8E, $9C, $FC, $BD, $1D, $5C));
      pid: 100);

//  Name:     System.Audio.PeakValue -- PKEY_Audio_PeakValue
//  Type:     UInt32 -- VT_UI4
//  FormatID: 2579E5D0-1116-4084-BD9A-9B4F7CB4DF5E, 100
  PKEY_Audio_PeakValue: TPropertyKey = (
      fmtid: (D1: $2579E5D0; D2: $1116; D3: $4084;
      D4: ($BD, $9A, $9B, $4F, $7C, $B4, $DF, $5E));
      pid: 100);

//  Name:     System.Audio.SampleRate -- PKEY_Audio_SampleRate
//  Type:     UInt32 -- VT_UI4
//  FormatID: (FMTID_AudioSummaryInformation) 64440490-4C8B-11D1-8B70-080036B11A03, 5 (PIDASI_SAMPLE_RATE)
//
//  Indicates the audio sample rate for the audio file in "samples per second".
  PKEY_Audio_SampleRate: TPropertyKey = (
      fmtid: (D1: $64440490; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 5);

//  Name:     System.Audio.SampleSize -- PKEY_Audio_SampleSize
//  Type:     UInt32 -- VT_UI4
//  FormatID: (FMTID_AudioSummaryInformation) 64440490-4C8B-11D1-8B70-080036B11A03, 6 (PIDASI_SAMPLE_SIZE)
//
//  Indicates the audio sample size for the audio file in "bits per sample".
  PKEY_Audio_SampleSize: TPropertyKey = (
      fmtid: (D1: $64440490; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 6);

//  Name:     System.Audio.StreamName -- PKEY_Audio_StreamName
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_AudioSummaryInformation) 64440490-4C8B-11D1-8B70-080036B11A03, 9 (PIDASI_STREAM_NAME)
//
//  
  PKEY_Audio_StreamName: TPropertyKey = (
      fmtid: (D1: $64440490; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 9);

//  Name:     System.Audio.StreamNumber -- PKEY_Audio_StreamNumber
//  Type:     UInt16 -- VT_UI2
//  FormatID: (FMTID_AudioSummaryInformation) 64440490-4C8B-11D1-8B70-080036B11A03, 8 (PIDASI_STREAM_NUMBER)
//
//  
  PKEY_Audio_StreamNumber: TPropertyKey = (
      fmtid: (D1: $64440490; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 8);

 
 
//-----------------------------------------------------------------------------
// Calendar properties

//  Name:     System.Calendar.Duration -- PKEY_Calendar_Duration
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 293CA35A-09AA-4DD2-B180-1FE245728A52, 100
//
//  The duration as specified in a string.
  PKEY_Calendar_Duration: TPropertyKey = (
      fmtid: (D1: $293CA35A; D2: $09AA; D3: $4DD2;
      D4: ($B1, $80, $1F, $E2, $45, $72, $8A, $52));
      pid: 100);

//  Name:     System.Calendar.IsOnline -- PKEY_Calendar_IsOnline
//  Type:     Boolean -- VT_BOOL
//  FormatID: BFEE9149-E3E2-49A7-A862-C05988145CEC, 100
//
//  Identifies if the event is an online event.
  PKEY_Calendar_IsOnline: TPropertyKey = (
      fmtid: (D1: $BFEE9149; D2: $E3E2; D3: $49A7;
      D4: ($A8, $62, $C0, $59, $88, $14, $5C, $EC));
      pid: 100);

//  Name:     System.Calendar.IsRecurring -- PKEY_Calendar_IsRecurring
//  Type:     Boolean -- VT_BOOL
//  FormatID: 315B9C8D-80A9-4EF9-AE16-8E746DA51D70, 100
  PKEY_Calendar_IsRecurring: TPropertyKey = (
      fmtid: (D1: $315B9C8D; D2: $80A9; D3: $4EF9;
      D4: ($AE, $16, $8E, $74, $6D, $A5, $1D, $70));
      pid: 100);

//  Name:     System.Calendar.Location -- PKEY_Calendar_Location
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: F6272D18-CECC-40B1-B26A-3911717AA7BD, 100
  PKEY_Calendar_Location: TPropertyKey = (
      fmtid: (D1: $F6272D18; D2: $CECC; D3: $40B1;
      D4: ($B2, $6A, $39, $11, $71, $7A, $A7, $BD));
      pid: 100);

//  Name:     System.Calendar.OptionalAttendeeAddresses -- PKEY_Calendar_OptionalAttendeeAddresses
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: D55BAE5A-3892-417A-A649-C6AC5AAAEAB3, 100
  PKEY_Calendar_OptionalAttendeeAddresses: TPropertyKey = (
      fmtid: (D1: $D55BAE5A; D2: $3892; D3: $417A;
      D4: ($A6, $49, $C6, $AC, $5A, $AA, $EA, $B3));
      pid: 100);

//  Name:     System.Calendar.OptionalAttendeeNames -- PKEY_Calendar_OptionalAttendeeNames
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: 09429607-582D-437F-84C3-DE93A2B24C3C, 100
  PKEY_Calendar_OptionalAttendeeNames: TPropertyKey = (
      fmtid: (D1: $09429607; D2: $582D; D3: $437F;
      D4: ($84, $C3, $DE, $93, $A2, $B2, $4C, $3C));
      pid: 100);

//  Name:     System.Calendar.OrganizerAddress -- PKEY_Calendar_OrganizerAddress
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 744C8242-4DF5-456C-AB9E-014EFB9021E3, 100
//
//  Address of the organizer organizing the event.
  PKEY_Calendar_OrganizerAddress: TPropertyKey = (
      fmtid: (D1: $744C8242; D2: $4DF5; D3: $456C;
      D4: ($AB, $9E, $01, $4E, $FB, $90, $21, $E3));
      pid: 100);

//  Name:     System.Calendar.OrganizerName -- PKEY_Calendar_OrganizerName
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: AAA660F9-9865-458E-B484-01BC7FE3973E, 100
//
//  Name of the organizer organizing the event.
  PKEY_Calendar_OrganizerName: TPropertyKey = (
      fmtid: (D1: $AAA660F9; D2: $9865; D3: $458E;
      D4: ($B4, $84, $01, $BC, $7F, $E3, $97, $3E));
      pid: 100);

//  Name:     System.Calendar.ReminderTime -- PKEY_Calendar_ReminderTime
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: 72FC5BA4-24F9-4011-9F3F-ADD27AFAD818, 100
  PKEY_Calendar_ReminderTime: TPropertyKey = (
      fmtid: (D1: $72FC5BA4; D2: $24F9; D3: $4011;
      D4: ($9F, $3F, $AD, $D2, $7A, $FA, $D8, $18));
      pid: 100);

//  Name:     System.Calendar.RequiredAttendeeAddresses -- PKEY_Calendar_RequiredAttendeeAddresses
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: 0BA7D6C3-568D-4159-AB91-781A91FB71E5, 100
  PKEY_Calendar_RequiredAttendeeAddresses: TPropertyKey = (
      fmtid: (D1: $0BA7D6C3; D2: $568D; D3: $4159;
      D4: ($AB, $91, $78, $1A, $91, $FB, $71, $E5));
      pid: 100);

//  Name:     System.Calendar.RequiredAttendeeNames -- PKEY_Calendar_RequiredAttendeeNames
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: B33AF30B-F552-4584-936C-CB93E5CDA29F, 100
  PKEY_Calendar_RequiredAttendeeNames: TPropertyKey = (
      fmtid: (D1: $B33AF30B; D2: $F552; D3: $4584;
      D4: ($93, $6C, $CB, $93, $E5, $CD, $A2, $9F));
      pid: 100);

//  Name:     System.Calendar.Resources -- PKEY_Calendar_Resources
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: 00F58A38-C54B-4C40-8696-97235980EAE1, 100
  PKEY_Calendar_Resources: TPropertyKey = (
      fmtid: (D1: $00F58A38; D2: $C54B; D3: $4C40;
      D4: ($86, $96, $97, $23, $59, $80, $EA, $E1));
      pid: 100);

//  Name:     System.Calendar.ShowTimeAs -- PKEY_Calendar_ShowTimeAs
//  Type:     UInt16 -- VT_UI2
//  FormatID: 5BF396D4-5EB2-466F-BDE9-2FB3F2361D6E, 100
//
//  
  PKEY_Calendar_ShowTimeAs: TPropertyKey = (
      fmtid: (D1: $5BF396D4; D2: $5EB2; D3: $466F;
      D4: ($BD, $E9, $2F, $B3, $F2, $36, $1D, $6E));
      pid: 100);

// Possible discrete values for PKEY_Calendar_ShowTimeAs are:
  CALENDAR_SHOWTIMEAS_FREE = 0;
  CALENDAR_SHOWTIMEAS_TENTATIVE = 1;
  CALENDAR_SHOWTIMEAS_BUSY = 2;
  CALENDAR_SHOWTIMEAS_OOF = 3;

//  Name:     System.Calendar.ShowTimeAsText -- PKEY_Calendar_ShowTimeAsText
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 53DA57CF-62C0-45C4-81DE-7610BCEFD7F5, 100
//  
//  This is the user-friendly form of System.Calendar.ShowTimeAs.  Not intended to be parsed 
//  programmatically.
  PKEY_Calendar_ShowTimeAsText: TPropertyKey = (
      fmtid: (D1: $53DA57CF; D2: $62C0; D3: $45C4;
      D4: ($81, $DE, $76, $10, $BC, $EF, $D7, $F5));
      pid: 100);
 
//-----------------------------------------------------------------------------
// Communication properties



//  Name:     System.Communication.AccountName -- PKEY_Communication_AccountName
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: E3E0584C-B788-4A5A-BB20-7F5A44C9ACDD, 9
//
//  Account Name
  PKEY_Communication_AccountName: TPropertyKey = (
      fmtid: (D1: $E3E0584C; D2: $B788; D3: $4A5A;
      D4: ($BB, $20, $7F, $5A, $44, $C9, $AC, $DD));
      pid: 9);

//  Name:     System.Communication.Suffix -- PKEY_Communication_Suffix
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 807B653A-9E91-43EF-8F97-11CE04EE20C5, 100
  PKEY_Communication_Suffix: TPropertyKey = (
      fmtid: (D1: $807B653A; D2: $9E91; D3: $43EF;
      D4: ($8F, $97, $11, $CE, $04, $EE, $20, $C5));
      pid: 100);

//  Name:     System.Communication.TaskStatus -- PKEY_Communication_TaskStatus
//  Type:     UInt16 -- VT_UI2
//  FormatID: BE1A72C6-9A1D-46B7-AFE7-AFAF8CEF4999, 100
  PKEY_Communication_TaskStatus: TPropertyKey = (
      fmtid: (D1: $BE1A72C6; D2: $9A1D; D3: $46B7;
      D4: ($AF, $E7, $AF, $AF, $8C, $EF, $49, $99));
      pid: 100);

// Possible discrete values for PKEY_Communication_TaskStatus are:
  TASKSTATUS_NOTSTARTED = 0;
  TASKSTATUS_INPROGRESS = 1;
  TASKSTATUS_COMPLETE = 2;
  TASKSTATUS_WAITING = 3;
  TASKSTATUS_DEFERRED = 4;

//  Name:     System.Communication.TaskStatusText -- PKEY_Communication_TaskStatusText
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: A6744477-C237-475B-A075-54F34498292A, 100
//  
//  This is the user-friendly form of System.Communication.TaskStatus.  Not intended to be parsed 
//  programmatically.
  PKEY_Communication_TaskStatusText: TPropertyKey = (
      fmtid: (D1: $A6744477; D2: $C237; D3: $475B;
      D4: ($A0, $75, $54, $F3, $44, $98, $29, $2A));
      pid: 100);
 
//-----------------------------------------------------------------------------
// Computer properties



//  Name:     System.Computer.DecoratedFreeSpace -- PKEY_Computer_DecoratedFreeSpace
//  Type:     Multivalue UInt64 -- VT_VECTOR | VT_UI8  (For variants: VT_ARRAY | VT_UI8)
//  FormatID: (FMTID_Volume) 9B174B35-40FF-11D2-A27E-00C04FC30871, 7  (Filesystem Volume Properties)
//
//  Free space and total space: "%s free of %s"
  PKEY_Computer_DecoratedFreeSpace: TPropertyKey = (
      fmtid: (D1: $9B174B35; D2: $40FF; D3: $11D2;
      D4: ($A2, $7E, $00, $C0, $4F, $C3, $08, $71));
      pid: 7);
 
//-----------------------------------------------------------------------------
// Contact properties



//  Name:     System.Contact.Anniversary -- PKEY_Contact_Anniversary
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: 9AD5BADB-CEA7-4470-A03D-B84E51B9949E, 100
  PKEY_Contact_Anniversary: TPropertyKey = (
      fmtid: (D1: $9AD5BADB; D2: $CEA7; D3: $4470;
      D4: ($A0, $3D, $B8, $4E, $51, $B9, $94, $9E));
      pid: 100);

//  Name:     System.Contact.AssistantName -- PKEY_Contact_AssistantName
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: CD102C9C-5540-4A88-A6F6-64E4981C8CD1, 100
  PKEY_Contact_AssistantName: TPropertyKey = (
      fmtid: (D1: $CD102C9C; D2: $5540; D3: $4A88;
      D4: ($A6, $F6, $64, $E4, $98, $1C, $8C, $D1));
      pid: 100);

//  Name:     System.Contact.AssistantTelephone -- PKEY_Contact_AssistantTelephone
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 9A93244D-A7AD-4FF8-9B99-45EE4CC09AF6, 100
  PKEY_Contact_AssistantTelephone: TPropertyKey = (
      fmtid: (D1: $9A93244D; D2: $A7AD; D3: $4FF8;
      D4: ($9B, $99, $45, $EE, $4C, $C0, $9A, $F6));
      pid: 100);

//  Name:     System.Contact.Birthday -- PKEY_Contact_Birthday
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: 176DC63C-2688-4E89-8143-A347800F25E9, 47
  PKEY_Contact_Birthday: TPropertyKey = (
      fmtid: (D1: $176DC63C; D2: $2688; D3: $4E89;
      D4: ($81, $43, $A3, $47, $80, $0F, $25, $E9));
      pid: 47);

//  Name:     System.Contact.BusinessAddress -- PKEY_Contact_BusinessAddress
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 730FB6DD-CF7C-426B-A03F-BD166CC9EE24, 100
  PKEY_Contact_BusinessAddress: TPropertyKey = (
      fmtid: (D1: $730FB6DD; D2: $CF7C; D3: $426B;
      D4: ($A0, $3F, $BD, $16, $6C, $C9, $EE, $24));
      pid: 100);

//  Name:     System.Contact.BusinessAddressCity -- PKEY_Contact_BusinessAddressCity
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 402B5934-EC5A-48C3-93E6-85E86A2D934E, 100
  PKEY_Contact_BusinessAddressCity: TPropertyKey = (
      fmtid: (D1: $402B5934; D2: $EC5A; D3: $48C3;
      D4: ($93, $E6, $85, $E8, $6A, $2D, $93, $4E));
      pid: 100);

//  Name:     System.Contact.BusinessAddressCountry -- PKEY_Contact_BusinessAddressCountry
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: B0B87314-FCF6-4FEB-8DFF-A50DA6AF561C, 100
  PKEY_Contact_BusinessAddressCountry: TPropertyKey = (
      fmtid: (D1: $B0B87314; D2: $FCF6; D3: $4FEB;
      D4: ($8D, $FF, $A5, $0D, $A6, $AF, $56, $1C));
      pid: 100);

//  Name:     System.Contact.BusinessAddressPostalCode -- PKEY_Contact_BusinessAddressPostalCode
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: E1D4A09E-D758-4CD1-B6EC-34A8B5A73F80, 100
  PKEY_Contact_BusinessAddressPostalCode: TPropertyKey = (
      fmtid: (D1: $E1D4A09E; D2: $D758; D3: $4CD1;
      D4: ($B6, $EC, $34, $A8, $B5, $A7, $3F, $80));
      pid: 100);

//  Name:     System.Contact.BusinessAddressPostOfficeBox -- PKEY_Contact_BusinessAddressPostOfficeBox
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: BC4E71CE-17F9-48D5-BEE9-021DF0EA5409, 100
  PKEY_Contact_BusinessAddressPostOfficeBox: TPropertyKey = (
      fmtid: (D1: $BC4E71CE; D2: $17F9; D3: $48D5;
      D4: ($BE, $E9, $02, $1D, $F0, $EA, $54, $09));
      pid: 100);

//  Name:     System.Contact.BusinessAddressState -- PKEY_Contact_BusinessAddressState
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 446F787F-10C4-41CB-A6C4-4D0343551597, 100
  PKEY_Contact_BusinessAddressState: TPropertyKey = (
      fmtid: (D1: $446F787F; D2: $10C4; D3: $41CB;
      D4: ($A6, $C4, $4D, $03, $43, $55, $15, $97));
      pid: 100);

//  Name:     System.Contact.BusinessAddressStreet -- PKEY_Contact_BusinessAddressStreet
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: DDD1460F-C0BF-4553-8CE4-10433C908FB0, 100
  PKEY_Contact_BusinessAddressStreet: TPropertyKey = (
      fmtid: (D1: $DDD1460F; D2: $C0BF; D3: $4553;
      D4: ($8C, $E4, $10, $43, $3C, $90, $8F, $B0));
      pid: 100);

//  Name:     System.Contact.BusinessFaxNumber -- PKEY_Contact_BusinessFaxNumber
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 91EFF6F3-2E27-42CA-933E-7C999FBE310B, 100
//
//  Business fax number of the contact.
  PKEY_Contact_BusinessFaxNumber: TPropertyKey = (
      fmtid: (D1: $91EFF6F3; D2: $2E27; D3: $42CA;
      D4: ($93, $3E, $7C, $99, $9F, $BE, $31, $0B));
      pid: 100);

//  Name:     System.Contact.BusinessHomePage -- PKEY_Contact_BusinessHomePage
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 56310920-2491-4919-99CE-EADB06FAFDB2, 100
  PKEY_Contact_BusinessHomePage: TPropertyKey = (
      fmtid: (D1: $56310920; D2: $2491; D3: $4919;
      D4: ($99, $CE, $EA, $DB, $06, $FA, $FD, $B2));
      pid: 100);

//  Name:     System.Contact.BusinessTelephone -- PKEY_Contact_BusinessTelephone
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 6A15E5A0-0A1E-4CD7-BB8C-D2F1B0C929BC, 100
  PKEY_Contact_BusinessTelephone: TPropertyKey = (
      fmtid: (D1: $6A15E5A0; D2: $0A1E; D3: $4CD7;
      D4: ($BB, $8C, $D2, $F1, $B0, $C9, $29, $BC));
      pid: 100);

//  Name:     System.Contact.CallbackTelephone -- PKEY_Contact_CallbackTelephone
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: BF53D1C3-49E0-4F7F-8567-5A821D8AC542, 100
  PKEY_Contact_CallbackTelephone: TPropertyKey = (
      fmtid: (D1: $BF53D1C3; D2: $49E0; D3: $4F7F;
      D4: ($85, $67, $5A, $82, $1D, $8A, $C5, $42));
      pid: 100);

//  Name:     System.Contact.CarTelephone -- PKEY_Contact_CarTelephone
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 8FDC6DEA-B929-412B-BA90-397A257465FE, 100
  PKEY_Contact_CarTelephone: TPropertyKey = (
      fmtid: (D1: $8FDC6DEA; D2: $B929; D3: $412B;
      D4: ($BA, $90, $39, $7A, $25, $74, $65, $FE));
      pid: 100);

//  Name:     System.Contact.Children -- PKEY_Contact_Children
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: D4729704-8EF1-43EF-9024-2BD381187FD5, 100
  PKEY_Contact_Children: TPropertyKey = (
      fmtid: (D1: $D4729704; D2: $8EF1; D3: $43EF;
      D4: ($90, $24, $2B, $D3, $81, $18, $7F, $D5));
      pid: 100);

//  Name:     System.Contact.CompanyMainTelephone -- PKEY_Contact_CompanyMainTelephone
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 8589E481-6040-473D-B171-7FA89C2708ED, 100
  PKEY_Contact_CompanyMainTelephone: TPropertyKey = (
      fmtid: (D1: $8589E481; D2: $6040; D3: $473D;
      D4: ($B1, $71, $7F, $A8, $9C, $27, $08, $ED));
      pid: 100);

//  Name:     System.Contact.Department -- PKEY_Contact_Department
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: FC9F7306-FF8F-4D49-9FB6-3FFE5C0951EC, 100
  PKEY_Contact_Department: TPropertyKey = (
      fmtid: (D1: $FC9F7306; D2: $FF8F; D3: $4D49;
      D4: ($9F, $B6, $3F, $FE, $5C, $09, $51, $EC));
      pid: 100);

//  Name:     System.Contact.EmailAddress -- PKEY_Contact_EmailAddress
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: F8FA7FA3-D12B-4785-8A4E-691A94F7A3E7, 100
  PKEY_Contact_EmailAddress: TPropertyKey = (
      fmtid: (D1: $F8FA7FA3; D2: $D12B; D3: $4785;
      D4: ($8A, $4E, $69, $1A, $94, $F7, $A3, $E7));
      pid: 100);

//  Name:     System.Contact.EmailAddress2 -- PKEY_Contact_EmailAddress2
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 38965063-EDC8-4268-8491-B7723172CF29, 100
  PKEY_Contact_EmailAddress2: TPropertyKey = (
      fmtid: (D1: $38965063; D2: $EDC8; D3: $4268;
      D4: ($84, $91, $B7, $72, $31, $72, $CF, $29));
      pid: 100);

//  Name:     System.Contact.EmailAddress3 -- PKEY_Contact_EmailAddress3
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 644D37B4-E1B3-4BAD-B099-7E7C04966ACA, 100
  PKEY_Contact_EmailAddress3: TPropertyKey = (
      fmtid: (D1: $644D37B4; D2: $E1B3; D3: $4BAD;
      D4: ($B0, $99, $7E, $7C, $04, $96, $6A, $CA));
      pid: 100);

//  Name:     System.Contact.EmailAddresses -- PKEY_Contact_EmailAddresses
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: 84D8F337-981D-44B3-9615-C7596DBA17E3, 100
  PKEY_Contact_EmailAddresses: TPropertyKey = (
      fmtid: (D1: $84D8F337; D2: $981D; D3: $44B3;
      D4: ($96, $15, $C7, $59, $6D, $BA, $17, $E3));
      pid: 100);

//  Name:     System.Contact.EmailName -- PKEY_Contact_EmailName
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: CC6F4F24-6083-4BD4-8754-674D0DE87AB8, 100
  PKEY_Contact_EmailName: TPropertyKey = (
      fmtid: (D1: $CC6F4F24; D2: $6083; D3: $4BD4;
      D4: ($87, $54, $67, $4D, $0D, $E8, $7A, $B8));
      pid: 100);

//  Name:     System.Contact.FileAsName -- PKEY_Contact_FileAsName
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: F1A24AA7-9CA7-40F6-89EC-97DEF9FFE8DB, 100
  PKEY_Contact_FileAsName: TPropertyKey = (
      fmtid: (D1: $F1A24AA7; D2: $9CA7; D3: $40F6;
      D4: ($89, $EC, $97, $DE, $F9, $FF, $E8, $DB));
      pid: 100);

//  Name:     System.Contact.FirstName -- PKEY_Contact_FirstName
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 14977844-6B49-4AAD-A714-A4513BF60460, 100
  PKEY_Contact_FirstName: TPropertyKey = (
      fmtid: (D1: $14977844; D2: $6B49; D3: $4AAD;
      D4: ($A7, $14, $A4, $51, $3B, $F6, $04, $60));
      pid: 100);

//  Name:     System.Contact.FullName -- PKEY_Contact_FullName
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 635E9051-50A5-4BA2-B9DB-4ED056C77296, 100
  PKEY_Contact_FullName: TPropertyKey = (
      fmtid: (D1: $635E9051; D2: $50A5; D3: $4BA2;
      D4: ($B9, $DB, $4E, $D0, $56, $C7, $72, $96));
      pid: 100);

//  Name:     System.Contact.Gender -- PKEY_Contact_Gender
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 3C8CEE58-D4F0-4CF9-B756-4E5D24447BCD, 100
  PKEY_Contact_Gender: TPropertyKey = (
      fmtid: (D1: $3C8CEE58; D2: $D4F0; D3: $4CF9;
      D4: ($B7, $56, $4E, $5D, $24, $44, $7B, $CD));
      pid: 100);

//  Name:     System.Contact.Hobbies -- PKEY_Contact_Hobbies
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: 5DC2253F-5E11-4ADF-9CFE-910DD01E3E70, 100
  PKEY_Contact_Hobbies: TPropertyKey = (
      fmtid: (D1: $5DC2253F; D2: $5E11; D3: $4ADF;
      D4: ($9C, $FE, $91, $0D, $D0, $1E, $3E, $70));
      pid: 100);

//  Name:     System.Contact.HomeAddress -- PKEY_Contact_HomeAddress
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 98F98354-617A-46B8-8560-5B1B64BF1F89, 100
  PKEY_Contact_HomeAddress: TPropertyKey = (
      fmtid: (D1: $98F98354; D2: $617A; D3: $46B8;
      D4: ($85, $60, $5B, $1B, $64, $BF, $1F, $89));
      pid: 100);

//  Name:     System.Contact.HomeAddressCity -- PKEY_Contact_HomeAddressCity
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 176DC63C-2688-4E89-8143-A347800F25E9, 65
  PKEY_Contact_HomeAddressCity: TPropertyKey = (
      fmtid: (D1: $176DC63C; D2: $2688; D3: $4E89;
      D4: ($81, $43, $A3, $47, $80, $0F, $25, $E9));
      pid: 65);

//  Name:     System.Contact.HomeAddressCountry -- PKEY_Contact_HomeAddressCountry
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 08A65AA1-F4C9-43DD-9DDF-A33D8E7EAD85, 100
  PKEY_Contact_HomeAddressCountry: TPropertyKey = (
      fmtid: (D1: $08A65AA1; D2: $F4C9; D3: $43DD;
      D4: ($9D, $DF, $A3, $3D, $8E, $7E, $AD, $85));
      pid: 100);

//  Name:     System.Contact.HomeAddressPostalCode -- PKEY_Contact_HomeAddressPostalCode
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 8AFCC170-8A46-4B53-9EEE-90BAE7151E62, 100
  PKEY_Contact_HomeAddressPostalCode: TPropertyKey = (
      fmtid: (D1: $8AFCC170; D2: $8A46; D3: $4B53;
      D4: ($9E, $EE, $90, $BA, $E7, $15, $1E, $62));
      pid: 100);

//  Name:     System.Contact.HomeAddressPostOfficeBox -- PKEY_Contact_HomeAddressPostOfficeBox
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 7B9F6399-0A3F-4B12-89BD-4ADC51C918AF, 100
  PKEY_Contact_HomeAddressPostOfficeBox: TPropertyKey = (
      fmtid: (D1: $7B9F6399; D2: $0A3F; D3: $4B12;
      D4: ($89, $BD, $4A, $DC, $51, $C9, $18, $AF));
      pid: 100);

//  Name:     System.Contact.HomeAddressState -- PKEY_Contact_HomeAddressState
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: C89A23D0-7D6D-4EB8-87D4-776A82D493E5, 100
  PKEY_Contact_HomeAddressState: TPropertyKey = (
      fmtid: (D1: $C89A23D0; D2: $7D6D; D3: $4EB8;
      D4: ($87, $D4, $77, $6A, $82, $D4, $93, $E5));
      pid: 100);

//  Name:     System.Contact.HomeAddressStreet -- PKEY_Contact_HomeAddressStreet
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 0ADEF160-DB3F-4308-9A21-06237B16FA2A, 100
  PKEY_Contact_HomeAddressStreet: TPropertyKey = (
      fmtid: (D1: $0ADEF160; D2: $DB3F; D3: $4308;
      D4: ($9A, $21, $06, $23, $7B, $16, $FA, $2A));
      pid: 100);

//  Name:     System.Contact.HomeFaxNumber -- PKEY_Contact_HomeFaxNumber
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 660E04D6-81AB-4977-A09F-82313113AB26, 100
  PKEY_Contact_HomeFaxNumber: TPropertyKey = (
      fmtid: (D1: $660E04D6; D2: $81AB; D3: $4977;
      D4: ($A0, $9F, $82, $31, $31, $13, $AB, $26));
      pid: 100);

//  Name:     System.Contact.HomeTelephone -- PKEY_Contact_HomeTelephone
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 176DC63C-2688-4E89-8143-A347800F25E9, 20
  PKEY_Contact_HomeTelephone: TPropertyKey = (
      fmtid: (D1: $176DC63C; D2: $2688; D3: $4E89;
      D4: ($81, $43, $A3, $47, $80, $0F, $25, $E9));
      pid: 20);

//  Name:     System.Contact.IMAddress -- PKEY_Contact_IMAddress
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: D68DBD8A-3374-4B81-9972-3EC30682DB3D, 100
  PKEY_Contact_IMAddress: TPropertyKey = (
      fmtid: (D1: $D68DBD8A; D2: $3374; D3: $4B81;
      D4: ($99, $72, $3E, $C3, $06, $82, $DB, $3D));
      pid: 100);

//  Name:     System.Contact.Initials -- PKEY_Contact_Initials
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: F3D8F40D-50CB-44A2-9718-40CB9119495D, 100
  PKEY_Contact_Initials: TPropertyKey = (
      fmtid: (D1: $F3D8F40D; D2: $50CB; D3: $44A2;
      D4: ($97, $18, $40, $CB, $91, $19, $49, $5D));
      pid: 100);

//  Name:     System.Contact.JA.CompanyNamePhonetic -- PKEY_Contact_JA_CompanyNamePhonetic
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 897B3694-FE9E-43E6-8066-260F590C0100, 2
//  
//  
  PKEY_Contact_JA_CompanyNamePhonetic: TPropertyKey = (
      fmtid: (D1: $897B3694; D2: $FE9E; D3: $43E6;
      D4: ($80, $66, $26, $0F, $59, $0C, $01, $00));
      pid: 2);

//  Name:     System.Contact.JA.FirstNamePhonetic -- PKEY_Contact_JA_FirstNamePhonetic
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 897B3694-FE9E-43E6-8066-260F590C0100, 3
//  
//  
  PKEY_Contact_JA_FirstNamePhonetic: TPropertyKey = (
      fmtid: (D1: $897B3694; D2: $FE9E; D3: $43E6;
      D4: ($80, $66, $26, $0F, $59, $0C, $01, $00));
      pid: 3);

//  Name:     System.Contact.JA.LastNamePhonetic -- PKEY_Contact_JA_LastNamePhonetic
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 897B3694-FE9E-43E6-8066-260F590C0100, 4
//  
//  
  PKEY_Contact_JA_LastNamePhonetic: TPropertyKey = (
      fmtid: (D1: $897B3694; D2: $FE9E; D3: $43E6;
      D4: ($80, $66, $26, $0F, $59, $0C, $01, $00));
      pid: 4);

//  Name:     System.Contact.JobTitle -- PKEY_Contact_JobTitle
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 176DC63C-2688-4E89-8143-A347800F25E9, 6
  PKEY_Contact_JobTitle: TPropertyKey = (
      fmtid: (D1: $176DC63C; D2: $2688; D3: $4E89;
      D4: ($81, $43, $A3, $47, $80, $0F, $25, $E9));
      pid: 6);

//  Name:     System.Contact.Label -- PKEY_Contact_Label
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 97B0AD89-DF49-49CC-834E-660974FD755B, 100
  PKEY_Contact_Label: TPropertyKey = (
      fmtid: (D1: $97B0AD89; D2: $DF49; D3: $49CC;
      D4: ($83, $4E, $66, $09, $74, $FD, $75, $5B));
      pid: 100);

//  Name:     System.Contact.LastName -- PKEY_Contact_LastName
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 8F367200-C270-457C-B1D4-E07C5BCD90C7, 100
  PKEY_Contact_LastName: TPropertyKey = (
      fmtid: (D1: $8F367200; D2: $C270; D3: $457C;
      D4: ($B1, $D4, $E0, $7C, $5B, $CD, $90, $C7));
      pid: 100);

//  Name:     System.Contact.MailingAddress -- PKEY_Contact_MailingAddress
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: C0AC206A-827E-4650-95AE-77E2BB74FCC9, 100
  PKEY_Contact_MailingAddress: TPropertyKey = (
      fmtid: (D1: $C0AC206A; D2: $827E; D3: $4650;
      D4: ($95, $AE, $77, $E2, $BB, $74, $FC, $C9));
      pid: 100);

//  Name:     System.Contact.MiddleName -- PKEY_Contact_MiddleName
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 176DC63C-2688-4E89-8143-A347800F25E9, 71
  PKEY_Contact_MiddleName: TPropertyKey = (
      fmtid: (D1: $176DC63C; D2: $2688; D3: $4E89;
      D4: ($81, $43, $A3, $47, $80, $0F, $25, $E9));
      pid: 71);

//  Name:     System.Contact.MobileTelephone -- PKEY_Contact_MobileTelephone
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 176DC63C-2688-4E89-8143-A347800F25E9, 35
  PKEY_Contact_MobileTelephone: TPropertyKey = (
      fmtid: (D1: $176DC63C; D2: $2688; D3: $4E89;
      D4: ($81, $43, $A3, $47, $80, $0F, $25, $E9));
      pid: 35);

//  Name:     System.Contact.NickName -- PKEY_Contact_NickName
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 176DC63C-2688-4E89-8143-A347800F25E9, 74
  PKEY_Contact_NickName: TPropertyKey = (
      fmtid: (D1: $176DC63C; D2: $2688; D3: $4E89;
      D4: ($81, $43, $A3, $47, $80, $0F, $25, $E9));
      pid: 74);

//  Name:     System.Contact.OfficeLocation -- PKEY_Contact_OfficeLocation
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 176DC63C-2688-4E89-8143-A347800F25E9, 7
  PKEY_Contact_OfficeLocation: TPropertyKey = (
      fmtid: (D1: $176DC63C; D2: $2688; D3: $4E89;
      D4: ($81, $43, $A3, $47, $80, $0F, $25, $E9));
      pid: 7);

//  Name:     System.Contact.OtherAddress -- PKEY_Contact_OtherAddress
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 508161FA-313B-43D5-83A1-C1ACCF68622C, 100
  PKEY_Contact_OtherAddress: TPropertyKey = (
      fmtid: (D1: $508161FA; D2: $313B; D3: $43D5;
      D4: ($83, $A1, $C1, $AC, $CF, $68, $62, $2C));
      pid: 100);

//  Name:     System.Contact.OtherAddressCity -- PKEY_Contact_OtherAddressCity
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 6E682923-7F7B-4F0C-A337-CFCA296687BF, 100
  PKEY_Contact_OtherAddressCity: TPropertyKey = (
      fmtid: (D1: $6E682923; D2: $7F7B; D3: $4F0C;
      D4: ($A3, $37, $CF, $CA, $29, $66, $87, $BF));
      pid: 100);

//  Name:     System.Contact.OtherAddressCountry -- PKEY_Contact_OtherAddressCountry
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 8F167568-0AAE-4322-8ED9-6055B7B0E398, 100
  PKEY_Contact_OtherAddressCountry: TPropertyKey = (
      fmtid: (D1: $8F167568; D2: $0AAE; D3: $4322;
      D4: ($8E, $D9, $60, $55, $B7, $B0, $E3, $98));
      pid: 100);

//  Name:     System.Contact.OtherAddressPostalCode -- PKEY_Contact_OtherAddressPostalCode
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 95C656C1-2ABF-4148-9ED3-9EC602E3B7CD, 100
  PKEY_Contact_OtherAddressPostalCode: TPropertyKey = (
      fmtid: (D1: $95C656C1; D2: $2ABF; D3: $4148;
      D4: ($9E, $D3, $9E, $C6, $02, $E3, $B7, $CD));
      pid: 100);

//  Name:     System.Contact.OtherAddressPostOfficeBox -- PKEY_Contact_OtherAddressPostOfficeBox
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 8B26EA41-058F-43F6-AECC-4035681CE977, 100
  PKEY_Contact_OtherAddressPostOfficeBox: TPropertyKey = (
      fmtid: (D1: $8B26EA41; D2: $058F; D3: $43F6;
      D4: ($AE, $CC, $40, $35, $68, $1C, $E9, $77));
      pid: 100);

//  Name:     System.Contact.OtherAddressState -- PKEY_Contact_OtherAddressState
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 71B377D6-E570-425F-A170-809FAE73E54E, 100
  PKEY_Contact_OtherAddressState: TPropertyKey = (
      fmtid: (D1: $71B377D6; D2: $E570; D3: $425F;
      D4: ($A1, $70, $80, $9F, $AE, $73, $E5, $4E));
      pid: 100);

//  Name:     System.Contact.OtherAddressStreet -- PKEY_Contact_OtherAddressStreet
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: FF962609-B7D6-4999-862D-95180D529AEA, 100
  PKEY_Contact_OtherAddressStreet: TPropertyKey = (
      fmtid: (D1: $FF962609; D2: $B7D6; D3: $4999;
      D4: ($86, $2D, $95, $18, $0D, $52, $9A, $EA));
      pid: 100);

//  Name:     System.Contact.PagerTelephone -- PKEY_Contact_PagerTelephone
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: D6304E01-F8F5-4F45-8B15-D024A6296789, 100
  PKEY_Contact_PagerTelephone: TPropertyKey = (
      fmtid: (D1: $D6304E01; D2: $F8F5; D3: $4F45;
      D4: ($8B, $15, $D0, $24, $A6, $29, $67, $89));
      pid: 100);

//  Name:     System.Contact.PersonalTitle -- PKEY_Contact_PersonalTitle
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 176DC63C-2688-4E89-8143-A347800F25E9, 69
  PKEY_Contact_PersonalTitle: TPropertyKey = (
      fmtid: (D1: $176DC63C; D2: $2688; D3: $4E89;
      D4: ($81, $43, $A3, $47, $80, $0F, $25, $E9));
      pid: 69);

//  Name:     System.Contact.PrimaryAddressCity -- PKEY_Contact_PrimaryAddressCity
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: C8EA94F0-A9E3-4969-A94B-9C62A95324E0, 100
  PKEY_Contact_PrimaryAddressCity: TPropertyKey = (
      fmtid: (D1: $C8EA94F0; D2: $A9E3; D3: $4969;
      D4: ($A9, $4B, $9C, $62, $A9, $53, $24, $E0));
      pid: 100);

//  Name:     System.Contact.PrimaryAddressCountry -- PKEY_Contact_PrimaryAddressCountry
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: E53D799D-0F3F-466E-B2FF-74634A3CB7A4, 100
  PKEY_Contact_PrimaryAddressCountry: TPropertyKey = (
      fmtid: (D1: $E53D799D; D2: $0F3F; D3: $466E;
      D4: ($B2, $FF, $74, $63, $4A, $3C, $B7, $A4));
      pid: 100);

//  Name:     System.Contact.PrimaryAddressPostalCode -- PKEY_Contact_PrimaryAddressPostalCode
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 18BBD425-ECFD-46EF-B612-7B4A6034EDA0, 100
  PKEY_Contact_PrimaryAddressPostalCode: TPropertyKey = (
      fmtid: (D1: $18BBD425; D2: $ECFD; D3: $46EF;
      D4: ($B6, $12, $7B, $4A, $60, $34, $ED, $A0));
      pid: 100);

//  Name:     System.Contact.PrimaryAddressPostOfficeBox -- PKEY_Contact_PrimaryAddressPostOfficeBox
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: DE5EF3C7-46E1-484E-9999-62C5308394C1, 100
  PKEY_Contact_PrimaryAddressPostOfficeBox: TPropertyKey = (
      fmtid: (D1: $DE5EF3C7; D2: $46E1; D3: $484E;
      D4: ($99, $99, $62, $C5, $30, $83, $94, $C1));
      pid: 100);

//  Name:     System.Contact.PrimaryAddressState -- PKEY_Contact_PrimaryAddressState
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: F1176DFE-7138-4640-8B4C-AE375DC70A6D, 100
  PKEY_Contact_PrimaryAddressState: TPropertyKey = (
      fmtid: (D1: $F1176DFE; D2: $7138; D3: $4640;
      D4: ($8B, $4C, $AE, $37, $5D, $C7, $0A, $6D));
      pid: 100);

//  Name:     System.Contact.PrimaryAddressStreet -- PKEY_Contact_PrimaryAddressStreet
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 63C25B20-96BE-488F-8788-C09C407AD812, 100
  PKEY_Contact_PrimaryAddressStreet: TPropertyKey = (
      fmtid: (D1: $63C25B20; D2: $96BE; D3: $488F;
      D4: ($87, $88, $C0, $9C, $40, $7A, $D8, $12));
      pid: 100);

//  Name:     System.Contact.PrimaryEmailAddress -- PKEY_Contact_PrimaryEmailAddress
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 176DC63C-2688-4E89-8143-A347800F25E9, 48
  PKEY_Contact_PrimaryEmailAddress: TPropertyKey = (
      fmtid: (D1: $176DC63C; D2: $2688; D3: $4E89;
      D4: ($81, $43, $A3, $47, $80, $0F, $25, $E9));
      pid: 48);

//  Name:     System.Contact.PrimaryTelephone -- PKEY_Contact_PrimaryTelephone
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 176DC63C-2688-4E89-8143-A347800F25E9, 25
  PKEY_Contact_PrimaryTelephone: TPropertyKey = (
      fmtid: (D1: $176DC63C; D2: $2688; D3: $4E89;
      D4: ($81, $43, $A3, $47, $80, $0F, $25, $E9));
      pid: 25);

//  Name:     System.Contact.Profession -- PKEY_Contact_Profession
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 7268AF55-1CE4-4F6E-A41F-B6E4EF10E4A9, 100
  PKEY_Contact_Profession: TPropertyKey = (
      fmtid: (D1: $7268AF55; D2: $1CE4; D3: $4F6E;
      D4: ($A4, $1F, $B6, $E4, $EF, $10, $E4, $A9));
      pid: 100);

//  Name:     System.Contact.SpouseName -- PKEY_Contact_SpouseName
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 9D2408B6-3167-422B-82B0-F583B7A7CFE3, 100
  PKEY_Contact_SpouseName: TPropertyKey = (
      fmtid: (D1: $9D2408B6; D2: $3167; D3: $422B;
      D4: ($82, $B0, $F5, $83, $B7, $A7, $CF, $E3));
      pid: 100);

//  Name:     System.Contact.Suffix -- PKEY_Contact_Suffix
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 176DC63C-2688-4E89-8143-A347800F25E9, 73
  PKEY_Contact_Suffix: TPropertyKey = (
      fmtid: (D1: $176DC63C; D2: $2688; D3: $4E89;
      D4: ($81, $43, $A3, $47, $80, $0F, $25, $E9));
      pid: 73);

//  Name:     System.Contact.TelexNumber -- PKEY_Contact_TelexNumber
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: C554493C-C1F7-40C1-A76C-EF8C0614003E, 100
  PKEY_Contact_TelexNumber: TPropertyKey = (
      fmtid: (D1: $C554493C; D2: $C1F7; D3: $40C1;
      D4: ($A7, $6C, $EF, $8C, $06, $14, $00, $3E));
      pid: 100);

//  Name:     System.Contact.TTYTDDTelephone -- PKEY_Contact_TTYTDDTelephone
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: AAF16BAC-2B55-45E6-9F6D-415EB94910DF, 100
  PKEY_Contact_TTYTDDTelephone: TPropertyKey = (
      fmtid: (D1: $AAF16BAC; D2: $2B55; D3: $45E6;
      D4: ($9F, $6D, $41, $5E, $B9, $49, $10, $DF));
      pid: 100);

//  Name:     System.Contact.WebPage -- PKEY_Contact_WebPage
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: E3E0584C-B788-4A5A-BB20-7F5A44C9ACDD, 18
  PKEY_Contact_WebPage: TPropertyKey = (
      fmtid: (D1: $E3E0584C; D2: $B788; D3: $4A5A;
      D4: ($BB, $20, $7F, $5A, $44, $C9, $AC, $DD));
      pid: 18);
 
//-----------------------------------------------------------------------------
// Core properties



//  Name:     System.AcquisitionID -- PKEY_AcquisitionID
//  Type:     Int32 -- VT_I4
//  FormatID: 65A98875-3C80-40AB-ABBC-EFDAF77DBEE2, 100
//
//  Hash to determine acquisition session.
  PKEY_AcquisitionID: TPropertyKey = (
      fmtid: (D1: $65A98875; D2: $3C80; D3: $40AB;
      D4: ($AB, $BC, $EF, $DA, $F7, $7D, $BE, $E2));
      pid: 100);

//  Name:     System.ApplicationName -- PKEY_ApplicationName
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)  Legacy code may treat this as VT_LPSTR.
//  FormatID: (FMTID_SummaryInformation) F29F85E0-4FF9-1068-AB91-08002B27B3D9, 18 (PIDSI_APPNAME)
//
//  
  PKEY_ApplicationName: TPropertyKey = (
      fmtid: (D1: $F29F85E0; D2: $4FF9; D3: $1068;
      D4: ($AB, $91, $08, $00, $2B, $27, $B3, $D9));
      pid: 18);

//  Name:     System.Author -- PKEY_Author
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)  Legacy code may treat this as VT_LPSTR.
//  FormatID: (FMTID_SummaryInformation) F29F85E0-4FF9-1068-AB91-08002B27B3D9, 4 (PIDSI_AUTHOR)
//
//  
  PKEY_Author: TPropertyKey = (
      fmtid: (D1: $F29F85E0; D2: $4FF9; D3: $1068;
      D4: ($AB, $91, $08, $00, $2B, $27, $B3, $D9));
      pid: 4);

//  Name:     System.Capacity -- PKEY_Capacity
//  Type:     UInt64 -- VT_UI8
//  FormatID: (FMTID_Volume) 9B174B35-40FF-11D2-A27E-00C04FC30871, 3 (PID_VOLUME_CAPACITY)  (Filesystem Volume Properties)
//
//  The amount of total space in bytes.
  PKEY_Capacity: TPropertyKey = (
      fmtid: (D1: $9B174B35; D2: $40FF; D3: $11D2;
      D4: ($A2, $7E, $00, $C0, $4F, $C3, $08, $71));
      pid: 3);

//  Name:     System.Category -- PKEY_Category
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: (FMTID_DocumentSummaryInformation) D5CDD502-2E9C-101B-9397-08002B2CF9AE, 2 (PIDDSI_CATEGORY)
//
//  Legacy code treats this as VT_LPSTR.
  PKEY_Category: TPropertyKey = (
      fmtid: (D1: $D5CDD502; D2: $2E9C; D3: $101B;
      D4: ($93, $97, $08, $00, $2B, $2C, $F9, $AE));
      pid: 2);

//  Name:     System.Comment -- PKEY_Comment
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)  Legacy code may treat this as VT_LPSTR.
//  FormatID: (FMTID_SummaryInformation) F29F85E0-4FF9-1068-AB91-08002B27B3D9, 6 (PIDSI_COMMENTS)
//
//  Comments.
  PKEY_Comment: TPropertyKey = (
      fmtid: (D1: $F29F85E0; D2: $4FF9; D3: $1068;
      D4: ($AB, $91, $08, $00, $2B, $27, $B3, $D9));
      pid: 6);

//  Name:     System.Company -- PKEY_Company
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_DocumentSummaryInformation) D5CDD502-2E9C-101B-9397-08002B2CF9AE, 15 (PIDDSI_COMPANY)
//
//  The company or publisher.
  PKEY_Company: TPropertyKey = (
      fmtid: (D1: $D5CDD502; D2: $2E9C; D3: $101B;
      D4: ($93, $97, $08, $00, $2B, $2C, $F9, $AE));
      pid: 15);

//  Name:     System.ComputerName -- PKEY_ComputerName
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_ShellDetails) 28636AA6-953D-11D2-B5D6-00C04FD918D0, 5 (PID_COMPUTERNAME)
//
//  
  PKEY_ComputerName: TPropertyKey = (
      fmtid: (D1: $28636AA6; D2: $953D; D3: $11D2;
      D4: ($B5, $D6, $00, $C0, $4F, $D9, $18, $D0));
      pid: 5);

//  Name:     System.ContainedItems -- PKEY_ContainedItems
//  Type:     Multivalue Guid -- VT_VECTOR | VT_CLSID  (For variants: VT_ARRAY | VT_CLSID)
//  FormatID: (FMTID_ShellDetails) 28636AA6-953D-11D2-B5D6-00C04FD918D0, 29
//  
//  The list of type of items, this item contains. For example, this item contains urls, attachments etc.
//  This is represented as a vector array of GUIDs where each GUID represents certain type.
  PKEY_ContainedItems: TPropertyKey = (
      fmtid: (D1: $28636AA6; D2: $953D; D3: $11D2;
      D4: ($B5, $D6, $00, $C0, $4F, $D9, $18, $D0));
      pid: 29);

//  Name:     System.ContentStatus -- PKEY_ContentStatus
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_DocumentSummaryInformation) D5CDD502-2E9C-101B-9397-08002B2CF9AE, 27
  PKEY_ContentStatus: TPropertyKey = (
      fmtid: (D1: $D5CDD502; D2: $2E9C; D3: $101B;
      D4: ($93, $97, $08, $00, $2B, $2C, $F9, $AE));
      pid: 27);

//  Name:     System.ContentType -- PKEY_ContentType
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_DocumentSummaryInformation) D5CDD502-2E9C-101B-9397-08002B2CF9AE, 26
  PKEY_ContentType: TPropertyKey = (
      fmtid: (D1: $D5CDD502; D2: $2E9C; D3: $101B;
      D4: ($93, $97, $08, $00, $2B, $2C, $F9, $AE));
      pid: 26);

//  Name:     System.Copyright -- PKEY_Copyright
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 11 (PIDMSI_COPYRIGHT)
//
//  
  PKEY_Copyright: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 11);

//  Name:     System.DateAccessed -- PKEY_DateAccessed
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: (FMTID_Storage) B725F130-47EF-101A-A5F1-02608C9EEBAC, 16 (PID_STG_ACCESSTIME)
//
//  The time of the last access to the item.  The Indexing Service friendly name is 'access'.
  PKEY_DateAccessed: TPropertyKey = (
      fmtid: (D1: $B725F130; D2: $47EF; D3: $101A;
      D4: ($A5, $F1, $02, $60, $8C, $9E, $EB, $AC));
      pid: 16);

//  Name:     System.DateAcquired -- PKEY_DateAcquired
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: 2CBAA8F5-D81F-47CA-B17A-F8D822300131, 100
//  
//  The time the file entered the system via acquisition.  This is not the same as System.DateImported.
//  Examples are when pictures are acquired from a camera, or when music is purchased online.
  PKEY_DateAcquired: TPropertyKey = (
      fmtid: (D1: $2CBAA8F5; D2: $D81F; D3: $47CA;
      D4: ($B1, $7A, $F8, $D8, $22, $30, $01, $31));
      pid: 100);

//  Name:     System.DateArchived -- PKEY_DateArchived
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: 43F8D7B7-A444-4F87-9383-52271C9B915C, 100
  PKEY_DateArchived: TPropertyKey = (
      fmtid: (D1: $43F8D7B7; D2: $A444; D3: $4F87;
      D4: ($93, $83, $52, $27, $1C, $9B, $91, $5C));
      pid: 100);

//  Name:     System.DateCompleted -- PKEY_DateCompleted
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: 72FAB781-ACDA-43E5-B155-B2434F85E678, 100
  PKEY_DateCompleted: TPropertyKey = (
      fmtid: (D1: $72FAB781; D2: $ACDA; D3: $43E5;
      D4: ($B1, $55, $B2, $43, $4F, $85, $E6, $78));
      pid: 100);

//  Name:     System.DateCreated -- PKEY_DateCreated
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: (FMTID_Storage) B725F130-47EF-101A-A5F1-02608C9EEBAC, 15 (PID_STG_CREATETIME)
//
//  The date and time the item was created. The Indexing Service friendly name is 'create'.
  PKEY_DateCreated: TPropertyKey = (
      fmtid: (D1: $B725F130; D2: $47EF; D3: $101A;
      D4: ($A5, $F1, $02, $60, $8C, $9E, $EB, $AC));
      pid: 15);

//  Name:     System.DateImported -- PKEY_DateImported
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: (FMTID_ImageProperties) 14B81DA1-0135-4D31-96D9-6CBFC9671A99, 18258
//
//  The time the file is imported into a separate database.  This is not the same as System.DateAcquired.  (Eg, 2003:05:22 13:55:04)
  PKEY_DateImported: TPropertyKey = (
      fmtid: (D1: $14B81DA1; D2: $0135; D3: $4D31;
      D4: ($96, $D9, $6C, $BF, $C9, $67, $1A, $99));
      pid: 18258);

//  Name:     System.DateModified -- PKEY_DateModified
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: (FMTID_Storage) B725F130-47EF-101A-A5F1-02608C9EEBAC, 14 (PID_STG_WRITETIME)
//
//  The date and time of the last write to the item. The Indexing Service friendly name is 'write'.
  PKEY_DateModified: TPropertyKey = (
      fmtid: (D1: $B725F130; D2: $47EF; D3: $101A;
      D4: ($A5, $F1, $02, $60, $8C, $9E, $EB, $AC));
      pid: 14);

//  Name:     System.DueDate -- PKEY_DueDate
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: 3F8472B5-E0AF-4DB2-8071-C53FE76AE7CE, 100
  PKEY_DueDate: TPropertyKey = (
      fmtid: (D1: $3F8472B5; D2: $E0AF; D3: $4DB2;
      D4: ($80, $71, $C5, $3F, $E7, $6A, $E7, $CE));
      pid: 100);

//  Name:     System.EndDate -- PKEY_EndDate
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: C75FAA05-96FD-49E7-9CB4-9F601082D553, 100
  PKEY_EndDate: TPropertyKey = (
      fmtid: (D1: $C75FAA05; D2: $96FD; D3: $49E7;
      D4: ($9C, $B4, $9F, $60, $10, $82, $D5, $53));
      pid: 100);

//  Name:     System.FileAllocationSize -- PKEY_FileAllocationSize
//  Type:     UInt64 -- VT_UI8
//  FormatID: (FMTID_Storage) B725F130-47EF-101A-A5F1-02608C9EEBAC, 18 (PID_STG_ALLOCSIZE)
//
//  
  PKEY_FileAllocationSize: TPropertyKey = (
      fmtid: (D1: $B725F130; D2: $47EF; D3: $101A;
      D4: ($A5, $F1, $02, $60, $8C, $9E, $EB, $AC));
      pid: 18);

//  Name:     System.FileAttributes -- PKEY_FileAttributes
//  Type:     UInt32 -- VT_UI4
//  FormatID: (FMTID_Storage) B725F130-47EF-101A-A5F1-02608C9EEBAC, 13 (PID_STG_ATTRIBUTES)
//  
//  This is the WIN32_FIND_DATA dwFileAttributes for the file-based item.
  PKEY_FileAttributes: TPropertyKey = (
      fmtid: (D1: $B725F130; D2: $47EF; D3: $101A;
      D4: ($A5, $F1, $02, $60, $8C, $9E, $EB, $AC));
      pid: 13);

//  Name:     System.FileCount -- PKEY_FileCount
//  Type:     UInt64 -- VT_UI8
//  FormatID: (FMTID_ShellDetails) 28636AA6-953D-11D2-B5D6-00C04FD918D0, 12
//
//  
  PKEY_FileCount: TPropertyKey = (
      fmtid: (D1: $28636AA6; D2: $953D; D3: $11D2;
      D4: ($B5, $D6, $00, $C0, $4F, $D9, $18, $D0));
      pid: 12);

//  Name:     System.FileDescription -- PKEY_FileDescription
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSFMTID_VERSION) 0CEF7D53-FA64-11D1-A203-0000F81FEDEE, 3 (PIDVSI_FileDescription)
//  
//  This is a user-friendly description of the file.
  PKEY_FileDescription: TPropertyKey = (
      fmtid: (D1: $0CEF7D53; D2: $FA64; D3: $11D1;
      D4: ($A2, $03, $00, $00, $F8, $1F, $ED, $EE));
      pid: 3);

//  Name:     System.FileExtension -- PKEY_FileExtension
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: E4F10A3C-49E6-405D-8288-A23BD4EEAA6C, 100
//  
//  This is the file extension of the file based item, including the leading period.  
//  
//  If System.FileName is VT_EMPTY, then this property should be too.  Otherwise, it should be derived
//  appropriately by the data source from System.FileName.  If System.FileName does not have a file 
//  extension, this value should be VT_EMPTY.
//  
//  To obtain the type of any item (including an item that is not a file), use System.ItemType.
//  
//  Example values:
//  
//      If the path is...                     The property value is...
//      -----------------                     ------------------------
//      "c:\foo\bar\hello.txt"                ".txt"
//      "\\server\share\mydir\goodnews.doc"   ".doc"
//      "\\server\share\numbers.xls"          ".xls"
//      "\\server\share\folder"               VT_EMPTY
//      "c:\foo\MyFolder"                     VT_EMPTY
//      [desktop]                             VT_EMPTY
  PKEY_FileExtension: TPropertyKey = (
      fmtid: (D1: $E4F10A3C; D2: $49E6; D3: $405D;
      D4: ($82, $88, $A2, $3B, $D4, $EE, $AA, $6C));
      pid: 100);

//  Name:     System.FileFRN -- PKEY_FileFRN
//  Type:     UInt64 -- VT_UI8
//  FormatID: (FMTID_Storage) B725F130-47EF-101A-A5F1-02608C9EEBAC, 21 (PID_STG_FRN)
//  
//  This is the unique file ID, also known as the File Reference Number. For a given file, this is the same value
//  as is found in the structure variable FILE_ID_BOTH_DIR_INFO.FileId, via GetFileInformationByHandleEx().
  PKEY_FileFRN: TPropertyKey = (
      fmtid: (D1: $B725F130; D2: $47EF; D3: $101A;
      D4: ($A5, $F1, $02, $60, $8C, $9E, $EB, $AC));
      pid: 21);

//  Name:     System.FileName -- PKEY_FileName
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 41CF5AE0-F75A-4806-BD87-59C7D9248EB9, 100
//  
//  This is the file name (including extension) of the file.
//  
//  It is possible that the item might not exist on a filesystem (ie, it may not be opened 
//  using CreateFile).  Nonetheless, if the item is represented as a file from the logical sense 
//  (and its name follows standard Win32 file-naming syntax), then the data source should emit this property.
//  
//  If an item is not a file, then the value for this property is VT_EMPTY.  See 
//  System.ItemNameDisplay.
//  
//  This has the same value as System.ParsingName for items that are provided by the Shell's file folder.
//  
//  Example values:
//  
//      If the path is...                     The property value is...
//      -----------------                     ------------------------
//      "c:\foo\bar\hello.txt"                "hello.txt"
//      "\\server\share\mydir\goodnews.doc"   "goodnews.doc"
//      "\\server\share\numbers.xls"          "numbers.xls"
//      "c:\foo\MyFolder"                     "MyFolder"
//      (email message)                       VT_EMPTY
//      (song on portable device)             "song.wma"
  PKEY_FileName: TPropertyKey = (
      fmtid: (D1: $41CF5AE0; D2: $F75A; D3: $4806;
      D4: ($BD, $87, $59, $C7, $D9, $24, $8E, $B9));
      pid: 100);

//  Name:     System.FileOwner -- PKEY_FileOwner
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_Misc) 9B174B34-40FF-11D2-A27E-00C04FC30871, 4 (PID_MISC_OWNER)
//  
//  This is the owner of the file, according to the file system.
  PKEY_FileOwner: TPropertyKey = (
      fmtid: (D1: $9B174B34; D2: $40FF; D3: $11D2;
      D4: ($A2, $7E, $00, $C0, $4F, $C3, $08, $71));
      pid: 4);

//  Name:     System.FileVersion -- PKEY_FileVersion
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSFMTID_VERSION) 0CEF7D53-FA64-11D1-A203-0000F81FEDEE, 4 (PIDVSI_FileVersion)
//
//  
  PKEY_FileVersion: TPropertyKey = (
      fmtid: (D1: $0CEF7D53; D2: $FA64; D3: $11D1;
      D4: ($A2, $03, $00, $00, $F8, $1F, $ED, $EE));
      pid: 4);

//  Name:     System.FindData -- PKEY_FindData
//  Type:     Buffer -- VT_VECTOR | VT_UI1  (For variants: VT_ARRAY | VT_UI1)
//  FormatID: (FMTID_ShellDetails) 28636AA6-953D-11D2-B5D6-00C04FD918D0, 0 (PID_FINDDATA)
//
//  WIN32_FIND_DATAW in buffer of bytes.
  PKEY_FindData: TPropertyKey = (
      fmtid: (D1: $28636AA6; D2: $953D; D3: $11D2;
      D4: ($B5, $D6, $00, $C0, $4F, $D9, $18, $D0));
      pid: 0);

//  Name:     System.FlagColor -- PKEY_FlagColor
//  Type:     UInt16 -- VT_UI2
//  FormatID: 67DF94DE-0CA7-4D6F-B792-053A3E4F03CF, 100
//
//  
  PKEY_FlagColor: TPropertyKey = (
      fmtid: (D1: $67DF94DE; D2: $0CA7; D3: $4D6F;
      D4: ($B7, $92, $05, $3A, $3E, $4F, $03, $CF));
      pid: 100);

// Possible discrete values for PKEY_FlagColor are:
  FLAGCOLOR_PURPLE = 1;
  FLAGCOLOR_ORANGE = 2;
  FLAGCOLOR_GREEN = 3;
  FLAGCOLOR_YELLOW = 4;
  FLAGCOLOR_BLUE = 5;
  FLAGCOLOR_RED = 6;

//  Name:     System.FlagColorText -- PKEY_FlagColorText
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 45EAE747-8E2A-40AE-8CBF-CA52ABA6152A, 100
//  
//  This is the user-friendly form of System.FlagColor.  Not intended to be parsed 
//  programmatically.
  PKEY_FlagColorText: TPropertyKey = (
      fmtid: (D1: $45EAE747; D2: $8E2A; D3: $40AE;
      D4: ($8C, $BF, $CA, $52, $AB, $A6, $15, $2A));
      pid: 100);

//  Name:     System.FlagStatus -- PKEY_FlagStatus
//  Type:     Int32 -- VT_I4
//  FormatID: E3E0584C-B788-4A5A-BB20-7F5A44C9ACDD, 12
//
//  Status of Flag.  Values: (0=none 1=white 2=Red).  cdoPR_FLAG_STATUS
  PKEY_FlagStatus: TPropertyKey = (
      fmtid: (D1: $E3E0584C; D2: $B788; D3: $4A5A;
      D4: ($BB, $20, $7F, $5A, $44, $C9, $AC, $DD));
      pid: 12);

// Possible discrete values for PKEY_FlagStatus are:
  FLAGSTATUS_NOTFLAGGED = 0;
  FLAGSTATUS_COMPLETED = 1;
  FLAGSTATUS_FOLLOWUP = 2;

//  Name:     System.FlagStatusText -- PKEY_FlagStatusText
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: DC54FD2E-189D-4871-AA01-08C2F57A4ABC, 100
//  
//  This is the user-friendly form of System.FlagStatus.  Not intended to be parsed 
//  programmatically.
  PKEY_FlagStatusText: TPropertyKey = (
      fmtid: (D1: $DC54FD2E; D2: $189D; D3: $4871;
      D4: ($AA, $01, $08, $C2, $F5, $7A, $4A, $BC));
      pid: 100);

//  Name:     System.FreeSpace -- PKEY_FreeSpace
//  Type:     UInt64 -- VT_UI8
//  FormatID: (FMTID_Volume) 9B174B35-40FF-11D2-A27E-00C04FC30871, 2 (PID_VOLUME_FREE)  (Filesystem Volume Properties)
//
//  The amount of free space in bytes.
  PKEY_FreeSpace: TPropertyKey = (
      fmtid: (D1: $9B174B35; D2: $40FF; D3: $11D2;
      D4: ($A2, $7E, $00, $C0, $4F, $C3, $08, $71));
      pid: 2);

//  Name:     System.Identity -- PKEY_Identity
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: A26F4AFC-7346-4299-BE47-EB1AE613139F, 100
  PKEY_Identity: TPropertyKey = (
      fmtid: (D1: $A26F4AFC; D2: $7346; D3: $4299;
      D4: ($BE, $47, $EB, $1A, $E6, $13, $13, $9F));
      pid: 100);

//  Name:     System.Importance -- PKEY_Importance
//  Type:     Int32 -- VT_I4
//  FormatID: E3E0584C-B788-4A5A-BB20-7F5A44C9ACDD, 11
  PKEY_Importance: TPropertyKey = (
      fmtid: (D1: $E3E0584C; D2: $B788; D3: $4A5A;
      D4: ($BB, $20, $7F, $5A, $44, $C9, $AC, $DD));
      pid: 11);

// Possible range of values for PKEY_Importance are:
  IMPORTANCE_LOW_MIN = 0;
  IMPORTANCE_LOW_SET = 1;
  IMPORTANCE_LOW_MAX = 1;

  IMPORTANCE_NORMAL_MIN = 2;
  IMPORTANCE_NORMAL_SET = 3;
  IMPORTANCE_NORMAL_MAX = 4;

  IMPORTANCE_HIGH_MIN = 5;
  IMPORTANCE_HIGH_SET = 5;
  IMPORTANCE_HIGH_MAX = 5;


//  Name:     System.ImportanceText -- PKEY_ImportanceText
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: A3B29791-7713-4E1D-BB40-17DB85F01831, 100
//  
//  This is the user-friendly form of System.Importance.  Not intended to be parsed 
//  programmatically.
  PKEY_ImportanceText: TPropertyKey = (
      fmtid: (D1: $A3B29791; D2: $7713; D3: $4E1D;
      D4: ($BB, $40, $17, $DB, $85, $F0, $18, $31));
      pid: 100);

//  Name:     System.IsAttachment -- PKEY_IsAttachment
//  Type:     Boolean -- VT_BOOL
//  FormatID: F23F425C-71A1-4FA8-922F-678EA4A60408, 100
//
//  Identifies if this item is an attachment.
  PKEY_IsAttachment: TPropertyKey = (
      fmtid: (D1: $F23F425C; D2: $71A1; D3: $4FA8;
      D4: ($92, $2F, $67, $8E, $A4, $A6, $04, $08));
      pid: 100);

//  Name:     System.IsDeleted -- PKEY_IsDeleted
//  Type:     Boolean -- VT_BOOL
//  FormatID: 5CDA5FC8-33EE-4FF3-9094-AE7BD8868C4D, 100
  PKEY_IsDeleted: TPropertyKey = (
      fmtid: (D1: $5CDA5FC8; D2: $33EE; D3: $4FF3;
      D4: ($90, $94, $AE, $7B, $D8, $86, $8C, $4D));
      pid: 100);

//  Name:     System.IsFlagged -- PKEY_IsFlagged
//  Type:     Boolean -- VT_BOOL
//  FormatID: 5DA84765-E3FF-4278-86B0-A27967FBDD03, 100
  PKEY_IsFlagged: TPropertyKey = (
      fmtid: (D1: $5DA84765; D2: $E3FF; D3: $4278;
      D4: ($86, $B0, $A2, $79, $67, $FB, $DD, $03));
      pid: 100);

//  Name:     System.IsFlaggedComplete -- PKEY_IsFlaggedComplete
//  Type:     Boolean -- VT_BOOL
//  FormatID: A6F360D2-55F9-48DE-B909-620E090A647C, 100
  PKEY_IsFlaggedComplete: TPropertyKey = (
      fmtid: (D1: $A6F360D2; D2: $55F9; D3: $48DE;
      D4: ($B9, $09, $62, $0E, $09, $0A, $64, $7C));
      pid: 100);

//  Name:     System.IsIncomplete -- PKEY_IsIncomplete
//  Type:     Boolean -- VT_BOOL
//  FormatID: 346C8BD1-2E6A-4C45-89A4-61B78E8E700F, 100
//
//  Identifies if the message was not completely received for some error condition.
  PKEY_IsIncomplete: TPropertyKey = (
      fmtid: (D1: $346C8BD1; D2: $2E6A; D3: $4C45;
      D4: ($89, $A4, $61, $B7, $8E, $8E, $70, $0F));
      pid: 100);

//  Name:     System.IsRead -- PKEY_IsRead
//  Type:     Boolean -- VT_BOOL
//  FormatID: E3E0584C-B788-4A5A-BB20-7F5A44C9ACDD, 10
//
//  Has the item been read?
  PKEY_IsRead: TPropertyKey = (
      fmtid: (D1: $E3E0584C; D2: $B788; D3: $4A5A;
      D4: ($BB, $20, $7F, $5A, $44, $C9, $AC, $DD));
      pid: 10);

//  Name:     System.IsSendToTarget -- PKEY_IsSendToTarget
//  Type:     Boolean -- VT_BOOL
//  FormatID: (FMTID_ShellDetails) 28636AA6-953D-11D2-B5D6-00C04FD918D0, 33
//
//  Provided by certain shell folders. Return TRUE if the folder is a valid Send To target.
  PKEY_IsSendToTarget: TPropertyKey = (
      fmtid: (D1: $28636AA6; D2: $953D; D3: $11D2;
      D4: ($B5, $D6, $00, $C0, $4F, $D9, $18, $D0));
      pid: 33);

//  Name:     System.IsShared -- PKEY_IsShared
//  Type:     Boolean -- VT_BOOL
//  FormatID: EF884C5B-2BFE-41BB-AAE5-76EEDF4F9902, 100
//
//  Is this item shared?
  PKEY_IsShared: TPropertyKey = (
      fmtid: (D1: $EF884C5B; D2: $2BFE; D3: $41BB;
      D4: ($AA, $E5, $76, $EE, $DF, $4F, $99, $02));
      pid: 100);

//  Name:     System.ItemAuthors -- PKEY_ItemAuthors
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: D0A04F0A-462A-48A4-BB2F-3706E88DBD7D, 100
//  
//  This is the generic list of authors associated with an item. 
//  
//  For example, the artist name for a track is the item author.
  PKEY_ItemAuthors: TPropertyKey = (
      fmtid: (D1: $D0A04F0A; D2: $462A; D3: $48A4;
      D4: ($BB, $2F, $37, $06, $E8, $8D, $BD, $7D));
      pid: 100);

//  Name:     System.ItemDate -- PKEY_ItemDate
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: F7DB74B4-4287-4103-AFBA-F1B13DCD75CF, 100
//  
//  This is the main date for an item. The date of interest. 
//  
//  For example, for photos this maps to System.Photo.DateTaken.
  PKEY_ItemDate: TPropertyKey = (
      fmtid: (D1: $F7DB74B4; D2: $4287; D3: $4103;
      D4: ($AF, $BA, $F1, $B1, $3D, $CD, $75, $CF));
      pid: 100);

//  Name:     System.ItemFolderNameDisplay -- PKEY_ItemFolderNameDisplay
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_Storage) B725F130-47EF-101A-A5F1-02608C9EEBAC, 2 (PID_STG_DIRECTORY)
//  
//  This is the user-friendly display name of the parent folder of an item.
//  
//  If System.ItemFolderPathDisplay is VT_EMPTY, then this property should be too.  Otherwise, it 
//  should be derived appropriately by the data source from System.ItemFolderPathDisplay.
//  
//  Example values:
//  
//      If the path is...                     The property value is...
//      -----------------                     ------------------------
//      "c:\foo\bar\hello.txt"                "bar"
//      "\\server\share\mydir\goodnews.doc"   "mydir"
//      "\\server\share\numbers.xls"          "share"
//      "c:\foo\MyFolder"                     "foo"
//      "/Mailbox Account/Inbox/'Re: Hello!'" "Inbox"
  PKEY_ItemFolderNameDisplay: TPropertyKey = (
      fmtid: (D1: $B725F130; D2: $47EF; D3: $101A;
      D4: ($A5, $F1, $02, $60, $8C, $9E, $EB, $AC));
      pid: 2);

//  Name:     System.ItemFolderPathDisplay -- PKEY_ItemFolderPathDisplay
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: E3E0584C-B788-4A5A-BB20-7F5A44C9ACDD, 6
//  
//  This is the user-friendly display path of the parent folder of an item.
//  
//  If System.ItemPathDisplay is VT_EMPTY, then this property should be too.  Otherwise, it should 
//  be derived appropriately by the data source from System.ItemPathDisplay.
//  
//  Example values:
//  
//      If the path is...                     The property value is...
//      -----------------                     ------------------------
//      "c:\foo\bar\hello.txt"                "c:\foo\bar"
//      "\\server\share\mydir\goodnews.doc"   "\\server\share\mydir"
//      "\\server\share\numbers.xls"          "\\server\share"
//      "c:\foo\MyFolder"                     "c:\foo"
//      "/Mailbox Account/Inbox/'Re: Hello!'" "/Mailbox Account/Inbox"
  PKEY_ItemFolderPathDisplay: TPropertyKey = (
      fmtid: (D1: $E3E0584C; D2: $B788; D3: $4A5A;
      D4: ($BB, $20, $7F, $5A, $44, $C9, $AC, $DD));
      pid: 6);

//  Name:     System.ItemFolderPathDisplayNarrow -- PKEY_ItemFolderPathDisplayNarrow
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: DABD30ED-0043-4789-A7F8-D013A4736622, 100
//  
//  This is the user-friendly display path of the parent folder of an item.  The format of the string
//  should be tailored such that the folder name comes first, to optimize for a narrow viewing column.
//  
//  If the folder is a file folder, the value includes localized names if they are present.
//  
//  If System.ItemFolderPathDisplay is VT_EMPTY, then this property should be too.  Otherwise, it should
//  be derived appropriately by the data source from System.ItemFolderPathDisplay.
//  
//  Example values:
//  
//      If the path is...                     The property value is...
//      -----------------                     ------------------------
//      "c:\foo\bar\hello.txt"                "bar (c:\foo)"
//      "\\server\share\mydir\goodnews.doc"   "mydir (\\server\share)"
//      "\\server\share\numbers.xls"          "share (\\server)"
//      "c:\foo\MyFolder"                     "foo (c:\)"
//      "/Mailbox Account/Inbox/'Re: Hello!'" "Inbox (/Mailbox Account)"
  PKEY_ItemFolderPathDisplayNarrow: TPropertyKey = (
      fmtid: (D1: $DABD30ED; D2: $0043; D3: $4789;
      D4: ($A7, $F8, $D0, $13, $A4, $73, $66, $22));
      pid: 100);

//  Name:     System.ItemName -- PKEY_ItemName
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 6B8DA074-3B5C-43BC-886F-0A2CDCE00B6F, 100
//  
//  This is the base-name of the System.ItemNameDisplay.
//  
//  If the item is a file this property
//  includes the extension in all cases, and will be localized if a localized name is available.
//  
//  If the item is a message, then the value of this property does not include the forwarding or
//  reply prefixes (see System.ItemNamePrefix).
  PKEY_ItemName: TPropertyKey = (
      fmtid: (D1: $6B8DA074; D2: $3B5C; D3: $43BC;
      D4: ($88, $6F, $0A, $2C, $DC, $E0, $0B, $6F));
      pid: 100);

//  Name:     System.ItemNameDisplay -- PKEY_ItemNameDisplay
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_Storage) B725F130-47EF-101A-A5F1-02608C9EEBAC, 10 (PID_STG_NAME)
//  
//  This is the display name in "most complete" form.  This is the best effort unique representation
//  of the name of an item that makes sense for end users to read.  It is the concatentation of
//  System.ItemNamePrefix and System.ItemName.
//  
//  If the item is a file this property
//  includes the extension in all cases, and will be localized if a localized name is available.
//  
//  There are acceptable cases when System.FileName is not VT_EMPTY, yet the value of this property 
//  is completely different.  Email messages are a key example.  If the item is an email message, 
//  the item name is likely the subject.  In that case, the value must be the concatenation of the
//  System.ItemNamePrefix and System.ItemName.  Since the value of System.ItemNamePrefix excludes
//  any trailing whitespace, the concatenation must include a whitespace when generating System.ItemNameDisplay.
//  
//  Note that this property is not guaranteed to be unique, but the idea is to promote the most likely
//  candidate that can be unique and also makes sense for end users. For example, for documents, you
//  might think about using System.Title as the System.ItemNameDisplay, but in practice the title of
//  the documents may not be useful or unique enough to be of value as the sole System.ItemNameDisplay.  
//  Instead, providing the value of System.FileName as the value of System.ItemNameDisplay is a better
//  candidate.  In Windows Mail, the emails are stored in the file system as .eml files and the 
//  System.FileName for those files are not human-friendly as they contain GUIDs. In this example, 
//  promoting System.Subject as System.ItemNameDisplay makes more sense.
//  
//  Compatibility notes:
//  
//  Shell folder implementations on Vista: use PKEY_ItemNameDisplay for the name column when
//  you want Explorer to call ISF::GetDisplayNameOf(SHGDN_NORMAL) to get the value of the name. Use
//  another PKEY (like PKEY_ItemName) when you want Explorer to call either the folder's property store or
//  ISF2::GetDetailsEx in order to get the value of the name.
//  
//  Shell folder implementations on XP: the first column needs to be the name column, and Explorer
//  will call ISF::GetDisplayNameOf to get the value of the name.  The PKEY/SCID does not matter.
//  
//  Example values:
//  
//      File:          "hello.txt"
//      Message:       "Re: Let's talk about Tom's argyle socks!"
//      Device folder: "song.wma"
//      Folder:        "Documents"
  PKEY_ItemNameDisplay: TPropertyKey = (
      fmtid: (D1: $B725F130; D2: $47EF; D3: $101A;
      D4: ($A5, $F1, $02, $60, $8C, $9E, $EB, $AC));
      pid: 10);

//  Name:     System.ItemNamePrefix -- PKEY_ItemNamePrefix
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: D7313FF1-A77A-401C-8C99-3DBDD68ADD36, 100
//  
//  This is the prefix of an item, used for email messages.
//  where the subject begins with "Re:" which is the prefix.
//  
//  If the item is a file, then the value of this property is VT_EMPTY.
//  
//  If the item is a message, then the value of this property is the forwarding or reply 
//  prefixes (including delimiting colon, but no whitespace), or VT_EMPTY if there is no prefix.
//  
//  Example values:
//  
//  System.ItemNamePrefix    System.ItemName      System.ItemNameDisplay
//  ---------------------    -------------------  ----------------------
//  VT_EMPTY                 "Great day"          "Great day"
//  "Re:"                    "Great day"          "Re: Great day"
//  "Fwd: "                  "Monthly budget"     "Fwd: Monthly budget"
//  VT_EMPTY                 "accounts.xls"       "accounts.xls"
  PKEY_ItemNamePrefix: TPropertyKey = (
      fmtid: (D1: $D7313FF1; D2: $A77A; D3: $401C;
      D4: ($8C, $99, $3D, $BD, $D6, $8A, $DD, $36));
      pid: 100);

//  Name:     System.ItemParticipants -- PKEY_ItemParticipants
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: D4D0AA16-9948-41A4-AA85-D97FF9646993, 100
//  
//  This is the generic list of people associated with an item and who contributed 
//  to the item. 
//  
//  For example, this is the combination of people in the To list, Cc list and 
//  sender of an email message.
  PKEY_ItemParticipants: TPropertyKey = (
      fmtid: (D1: $D4D0AA16; D2: $9948; D3: $41A4;
      D4: ($AA, $85, $D9, $7F, $F9, $64, $69, $93));
      pid: 100);

//  Name:     System.ItemPathDisplay -- PKEY_ItemPathDisplay
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: E3E0584C-B788-4A5A-BB20-7F5A44C9ACDD, 7
//  
//  This is the user-friendly display path to the item.
//  
//  If the item is a file or folder this property
//  includes the extension in all cases, and will be localized if a localized name is available.
//  
//  For other items,this is the user-friendly equivalent, assuming the item exists in hierarchical storage.
//  
//  Unlike System.ItemUrl, this property value does not include the URL scheme.
//  
//  To parse an item path, use System.ItemUrl or System.ParsingPath.  To reference shell 
//  namespace items using shell APIs, use System.ParsingPath.
//  
//  Example values:
//  
//      If the path is...                     The property value is...
//      -----------------                     ------------------------
//      "c:\foo\bar\hello.txt"                "c:\foo\bar\hello.txt"
//      "\\server\share\mydir\goodnews.doc"   "\\server\share\mydir\goodnews.doc"
//      "\\server\share\numbers.xls"          "\\server\share\numbers.xls"
//      "c:\foo\MyFolder"                     "c:\foo\MyFolder"
//      "/Mailbox Account/Inbox/'Re: Hello!'" "/Mailbox Account/Inbox/'Re: Hello!'"
  PKEY_ItemPathDisplay: TPropertyKey = (
      fmtid: (D1: $E3E0584C; D2: $B788; D3: $4A5A;
      D4: ($BB, $20, $7F, $5A, $44, $C9, $AC, $DD));
      pid: 7);

//  Name:     System.ItemPathDisplayNarrow -- PKEY_ItemPathDisplayNarrow
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_ShellDetails) 28636AA6-953D-11D2-B5D6-00C04FD918D0, 8
//  
//  This is the user-friendly display path to the item. The format of the string should be 
//  tailored such that the name comes first, to optimize for a narrow viewing column.
//  
//  If the item is a file, the value excludes the file extension, and includes localized names if they are present.
//  If the item is a message, the value includes the System.ItemNamePrefix.
//  
//  To parse an item path, use System.ItemUrl or System.ParsingPath.
//  
//  Example values:
//  
//      If the path is...                     The property value is...
//      -----------------                     ------------------------
//      "c:\foo\bar\hello.txt"                "hello (c:\foo\bar)"
//      "\\server\share\mydir\goodnews.doc"   "goodnews (\\server\share\mydir)"
//      "\\server\share\folder"               "folder (\\server\share)"
//      "c:\foo\MyFolder"                     "MyFolder (c:\foo)"
//      "/Mailbox Account/Inbox/'Re: Hello!'" "Re: Hello! (/Mailbox Account/Inbox)"
  PKEY_ItemPathDisplayNarrow: TPropertyKey = (
      fmtid: (D1: $28636AA6; D2: $953D; D3: $11D2;
      D4: ($B5, $D6, $00, $C0, $4F, $D9, $18, $D0));
      pid: 8);

//  Name:     System.ItemType -- PKEY_ItemType
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_ShellDetails) 28636AA6-953D-11D2-B5D6-00C04FD918D0, 11
//  
//  This is the canonical type of the item and is intended to be programmatically
//  parsed.
//  
//  If there is no canonical type, the value is VT_EMPTY.
//  
//  If the item is a file (ie, System.FileName is not VT_EMPTY), the value is the same as
//  System.FileExtension.
//  
//  Use System.ItemTypeText when you want to display the type to end users in a view.  (If
//   the item is a file, passing the System.ItemType value to PSFormatForDisplay will
//   result in the same value as System.ItemTypeText.)
//  
//  Example values:
//  
//      If the path is...                     The property value is...
//      -----------------                     ------------------------
//      "c:\foo\bar\hello.txt"                ".txt"
//      "\\server\share\mydir\goodnews.doc"   ".doc"
//      "\\server\share\folder"               "Directory"
//      "c:\foo\MyFolder"                     "Directory"
//      [desktop]                             "Folder"
//      "/Mailbox Account/Inbox/'Re: Hello!'" "MAPI/IPM.Message"
  PKEY_ItemType: TPropertyKey = (
      fmtid: (D1: $28636AA6; D2: $953D; D3: $11D2;
      D4: ($B5, $D6, $00, $C0, $4F, $D9, $18, $D0));
      pid: 11);

//  Name:     System.ItemTypeText -- PKEY_ItemTypeText
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_Storage) B725F130-47EF-101A-A5F1-02608C9EEBAC, 4 (PID_STG_STORAGETYPE)
//  
//  This is the user friendly type name of the item.  This is not intended to be
//  programmatically parsed.
//  
//  If System.ItemType is VT_EMPTY, the value of this property is also VT_EMPTY.
//  
//  If the item is a file, the value of this property is the same as if you passed the 
//  file's System.ItemType value to PSFormatForDisplay.
//  
//  This property should not be confused with System.Kind, where System.Kind is a high-level
//  user friendly kind name. For example, for a document, System.Kind = "Document" and 
//  System.Item.Type = ".doc" and System.Item.TypeText = "Microsoft Word Document"
//  
//  Example values:
//  
//      If the path is...                     The property value is...
//      -----------------                     ------------------------
//      "c:\foo\bar\hello.txt"                "Text File"
//      "\\server\share\mydir\goodnews.doc"   "Microsoft Word Document"
//      "\\server\share\folder"               "File Folder"
//      "c:\foo\MyFolder"                     "File Folder"
//      "/Mailbox Account/Inbox/'Re: Hello!'" "Outlook E-Mail Message"
  PKEY_ItemTypeText: TPropertyKey = (
      fmtid: (D1: $B725F130; D2: $47EF; D3: $101A;
      D4: ($A5, $F1, $02, $60, $8C, $9E, $EB, $AC));
      pid: 4);

//  Name:     System.ItemUrl -- PKEY_ItemUrl
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_Query) 49691C90-7E17-101A-A91C-08002B2ECDA9, 9 (PROPID_QUERY_VIRTUALPATH)
//  
//  This always represents a well formed URL that points to the item.  
//  
//  To reference shell namespace items using shell APIs, use System.ParsingPath.
//  
//  Example values:
//  
//      Files:    "file:///c:/foo/bar/hello.txt"
//                "csc://{GUID}/..."
//      Messages: "mapi://..."
  PKEY_ItemUrl: TPropertyKey = (
      fmtid: (D1: $49691C90; D2: $7E17; D3: $101A;
      D4: ($A9, $1C, $08, $00, $2B, $2E, $CD, $A9));
      pid: 9);

//  Name:     System.Keywords -- PKEY_Keywords
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)  Legacy code may treat this as VT_LPSTR.
//  FormatID: (FMTID_SummaryInformation) F29F85E0-4FF9-1068-AB91-08002B27B3D9, 5 (PIDSI_KEYWORDS)
//
//  The keywords for the item.  Also referred to as tags.
  PKEY_Keywords: TPropertyKey = (
      fmtid: (D1: $F29F85E0; D2: $4FF9; D3: $1068;
      D4: ($AB, $91, $08, $00, $2B, $27, $B3, $D9));
      pid: 5);

//  Name:     System.Kind -- PKEY_Kind
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: 1E3EE840-BC2B-476C-8237-2ACD1A839B22, 3
//  
//  System.Kind is used to map extensions to various .Search folders.
//  Extensions are mapped to Kinds at HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion\Explorer\KindMap
//  The list of kinds is not extensible.
  PKEY_Kind: TPropertyKey = (
      fmtid: (D1: $1E3EE840; D2: $BC2B; D3: $476C;
      D4: ($82, $37, $2A, $CD, $1A, $83, $9B, $22));
      pid: 3);

// Possible discrete values for PKEY_Kind are:
  KIND_CALENDAR: PWideChar = 'calendar';
  KIND_COMMUNICATION: PWideChar = 'communication';
  KIND_CONTACT: PWideChar = 'contact';
  KIND_DOCUMENT: PWideChar = 'document';
  KIND_EMAIL: PWideChar = 'emai';
  KIND_FEED: PWideChar = 'feed';
  KIND_FOLDER: PWideChar = 'folder';
  KIND_GAME: PWideChar = 'game';
  KIND_INSTANTMESSAGE: PWideChar = 'instantmessage';
  KIND_JOURNAL: PWideChar = 'journa';
  KIND_LINK: PWideChar = 'link';
  KIND_MOVIE: PWideChar = 'movie';
  KIND_MUSIC: PWideChar = 'music';
  KIND_NOTE: PWideChar = 'note';
  KIND_PICTURE: PWideChar = 'picture';
  KIND_PROGRAM: PWideChar = 'program';
  KIND_RECORDEDTV: PWideChar = 'recordedtv';
  KIND_SEARCHFOLDER: PWideChar = 'searchfolder';
  KIND_TASK: PWideChar = 'task';
  KIND_VIDEO: PWideChar = 'video';
  KIND_WEBHISTORY: PWideChar = 'webhistory';

//  Name:     System.KindText -- PKEY_KindText
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: F04BEF95-C585-4197-A2B7-DF46FDC9EE6D, 100
//  
//  This is the user-friendly form of System.Kind.  Not intended to be parsed 
//  programmatically.
  PKEY_KindText: TPropertyKey = (
      fmtid: (D1: $F04BEF95; D2: $C585; D3: $4197;
      D4: ($A2, $B7, $DF, $46, $FD, $C9, $EE, $6D));
      pid: 100);

//  Name:     System.Language -- PKEY_Language
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_DocumentSummaryInformation) D5CDD502-2E9C-101B-9397-08002B2CF9AE, 28
//
//  
  PKEY_Language: TPropertyKey = (
      fmtid: (D1: $D5CDD502; D2: $2E9C; D3: $101B;
      D4: ($93, $97, $08, $00, $2B, $2C, $F9, $AE));
      pid: 28);

//  Name:     System.MileageInformation -- PKEY_MileageInformation
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: FDF84370-031A-4ADD-9E91-0D775F1C6605, 100
  PKEY_MileageInformation: TPropertyKey = (
      fmtid: (D1: $FDF84370; D2: $031A; D3: $4ADD;
      D4: ($9E, $91, $0D, $77, $5F, $1C, $66, $05));
      pid: 100);

//  Name:     System.MIMEType -- PKEY_MIMEType
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 0B63E350-9CCC-11D0-BCDB-00805FCCCE04, 5
//
//  The MIME type.  Eg, for EML files: 'message/rfc822'.
  PKEY_MIMEType: TPropertyKey = (
      fmtid: (D1: $0B63E350; D2: $9CCC; D3: $11D0;
      D4: ($BC, $DB, $00, $80, $5F, $CC, $CE, $04));
      pid: 5);

//  Name:     System.Null -- PKEY_Null
//  Type:     Null -- VT_NULL
//  FormatID: 00000000-0000-0000-0000-000000000000, 0
  PKEY_Null: TPropertyKey = (
      fmtid: (D1: $00000000; D2: $0000; D3: $0000;
      D4: ($00, $00, $00, $00, $00, $00, $00, $00));
      pid: 0);

//  Name:     System.OfflineAvailability -- PKEY_OfflineAvailability
//  Type:     UInt32 -- VT_UI4
//  FormatID: A94688B6-7D9F-4570-A648-E3DFC0AB2B3F, 100
  PKEY_OfflineAvailability: TPropertyKey = (
      fmtid: (D1: $A94688B6; D2: $7D9F; D3: $4570;
      D4: ($A6, $48, $E3, $DF, $C0, $AB, $2B, $3F));
      pid: 100);

// Possible discrete values for PKEY_OfflineAvailability are:
  OFFLINEAVAILABILITY_NOT_AVAILABLE = 0;
  OFFLINEAVAILABILITY_AVAILABLE = 1;
  OFFLINEAVAILABILITY_ALWAYS_AVAILABLE = 2;

//  Name:     System.OfflineStatus -- PKEY_OfflineStatus
//  Type:     UInt32 -- VT_UI4
//  FormatID: 6D24888F-4718-4BDA-AFED-EA0FB4386CD8, 100
  PKEY_OfflineStatus: TPropertyKey = (
      fmtid: (D1: $6D24888F; D2: $4718; D3: $4BDA;
      D4: ($AF, $ED, $EA, $0F, $B4, $38, $6C, $D8));
      pid: 100);

// Possible discrete values for PKEY_OfflineStatus are:
  OFFLINESTATUS_ONLINE = 0;
  OFFLINESTATUS_OFFLINE = 1;
  OFFLINESTATUS_OFFLINE_FORCED = 2;
  OFFLINESTATUS_OFFLINE_SLOW = 3;
  OFFLINESTATUS_OFFLINE_ERROR = 4;
  OFFLINESTATUS_OFFLINE_ITEM_VERSION_CONFLICT = 5;
  OFFLINESTATUS_OFFLINE_SUSPENDED = 6;

//  Name:     System.OriginalFileName -- PKEY_OriginalFileName
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSFMTID_VERSION) 0CEF7D53-FA64-11D1-A203-0000F81FEDEE, 6
//  
//  
  PKEY_OriginalFileName: TPropertyKey = (
      fmtid: (D1: $0CEF7D53; D2: $FA64; D3: $11D1;
      D4: ($A2, $03, $00, $00, $F8, $1F, $ED, $EE));
      pid: 6);

//  Name:     System.ParentalRating -- PKEY_ParentalRating
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 21 (PIDMSI_PARENTAL_RATING)
//
//  
  PKEY_ParentalRating: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 21);

//  Name:     System.ParentalRatingReason -- PKEY_ParentalRatingReason
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 10984E0A-F9F2-4321-B7EF-BAF195AF4319, 100
  PKEY_ParentalRatingReason: TPropertyKey = (
      fmtid: (D1: $10984E0A; D2: $F9F2; D3: $4321;
      D4: ($B7, $EF, $BA, $F1, $95, $AF, $43, $19));
      pid: 100);

//  Name:     System.ParentalRatingsOrganization -- PKEY_ParentalRatingsOrganization
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: A7FE0840-1344-46F0-8D37-52ED712A4BF9, 100
  PKEY_ParentalRatingsOrganization: TPropertyKey = (
      fmtid: (D1: $A7FE0840; D2: $1344; D3: $46F0;
      D4: ($8D, $37, $52, $ED, $71, $2A, $4B, $F9));
      pid: 100);

//  Name:     System.ParsingBindContext -- PKEY_ParsingBindContext
//  Type:     Any -- VT_NULL  Legacy code may treat this as VT_UNKNOWN.
//  FormatID: DFB9A04D-362F-4CA3-B30B-0254B17B5B84, 100
//  
//  used to get the IBindCtx for an item for parsing
  PKEY_ParsingBindContext: TPropertyKey = (
      fmtid: (D1: $DFB9A04D; D2: $362F; D3: $4CA3;
      D4: ($B3, $0B, $02, $54, $B1, $7B, $5B, $84));
      pid: 100);

//  Name:     System.ParsingName -- PKEY_ParsingName
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_ShellDetails) 28636AA6-953D-11D2-B5D6-00C04FD918D0, 24
//  
//  The shell namespace name of an item relative to a parent folder.  This name may be passed to 
//  IShellFolder::ParseDisplayName() of the parent shell folder.
  PKEY_ParsingName: TPropertyKey = (
      fmtid: (D1: $28636AA6; D2: $953D; D3: $11D2;
      D4: ($B5, $D6, $00, $C0, $4F, $D9, $18, $D0));
      pid: 24);

//  Name:     System.ParsingPath -- PKEY_ParsingPath
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_ShellDetails) 28636AA6-953D-11D2-B5D6-00C04FD918D0, 30
//  
//  This is the shell namespace path to the item.  This path may be passed to 
//  SHParseDisplayName to parse the path to the correct shell folder.
//  
//  If the item is a file, the value is identical to System.ItemPathDisplay.
//  
//  If the item cannot be accessed through the shell namespace, this value is VT_EMPTY.
  PKEY_ParsingPath: TPropertyKey = (
      fmtid: (D1: $28636AA6; D2: $953D; D3: $11D2;
      D4: ($B5, $D6, $00, $C0, $4F, $D9, $18, $D0));
      pid: 30);

//  Name:     System.PerceivedType -- PKEY_PerceivedType
//  Type:     Int32 -- VT_I4
//  FormatID: (FMTID_ShellDetails) 28636AA6-953D-11D2-B5D6-00C04FD918D0, 9
//
//  The perceived type of a shell item, based upon its canonical type.
  PKEY_PerceivedType: TPropertyKey = (
      fmtid: (D1: $28636AA6; D2: $953D; D3: $11D2;
      D4: ($B5, $D6, $00, $C0, $4F, $D9, $18, $D0));
      pid: 9);

// For the enumerated values of PKEY_PerceivedType, see the PERCEIVED_TYPE_* values in shtypes.idl.

//  Name:     System.PercentFull -- PKEY_PercentFull
//  Type:     UInt32 -- VT_UI4
//  FormatID: (FMTID_Volume) 9B174B35-40FF-11D2-A27E-00C04FC30871, 5  (Filesystem Volume Properties)
//
//  The amount filled as a percentage, multiplied by 100 (ie, the valid range is 0 through 100).
  PKEY_PercentFull: TPropertyKey = (
      fmtid: (D1: $9B174B35; D2: $40FF; D3: $11D2;
      D4: ($A2, $7E, $00, $C0, $4F, $C3, $08, $71));
      pid: 5);

//  Name:     System.Priority -- PKEY_Priority
//  Type:     UInt16 -- VT_UI2
//  FormatID: 9C1FCF74-2D97-41BA-B4AE-CB2E3661A6E4, 5
//
//  
  PKEY_Priority: TPropertyKey = (
      fmtid: (D1: $9C1FCF74; D2: $2D97; D3: $41BA;
      D4: ($B4, $AE, $CB, $2E, $36, $61, $A6, $E4));
      pid: 5);

// Possible discrete values for PKEY_Priority are:
  PRIORITY_PROP_LOW = 0;
  PRIORITY_PROP_NORMAL = 1;
  PRIORITY_PROP_HIGH = 2;

//  Name:     System.PriorityText -- PKEY_PriorityText
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: D98BE98B-B86B-4095-BF52-9D23B2E0A752, 100
//  
//  This is the user-friendly form of System.Priority.  Not intended to be parsed 
//  programmatically.
  PKEY_PriorityText: TPropertyKey = (
      fmtid: (D1: $D98BE98B; D2: $B86B; D3: $4095;
      D4: ($BF, $52, $9D, $23, $B2, $E0, $A7, $52));
      pid: 100);

//  Name:     System.Project -- PKEY_Project
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 39A7F922-477C-48DE-8BC8-B28441E342E3, 100
  PKEY_Project: TPropertyKey = (
      fmtid: (D1: $39A7F922; D2: $477C; D3: $48DE;
      D4: ($8B, $C8, $B2, $84, $41, $E3, $42, $E3));
      pid: 100);

//  Name:     System.ProviderItemID -- PKEY_ProviderItemID
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: F21D9941-81F0-471A-ADEE-4E74B49217ED, 100
//  
//  
  PKEY_ProviderItemID: TPropertyKey = (
      fmtid: (D1: $F21D9941; D2: $81F0; D3: $471A;
      D4: ($AD, $EE, $4E, $74, $B4, $92, $17, $ED));
      pid: 100);

//  Name:     System.Rating -- PKEY_Rating
//  Type:     UInt32 -- VT_UI4
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 9 (PIDMSI_RATING)
//  
//  Indicates the users preference rating of an item on a scale of 0-99 (0 = unrated, 1-12 = One Star, 
//  13-37 = Two Stars, 38-62 = Three Stars, 63-87 = Four Stars, 88-99 = Five Stars).
  PKEY_Rating: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 9);

// Use the following constants to convert between visual stars and the ratings value:
  RATING_UNRATED_MIN = 0;
  RATING_UNRATED_SET = 0;
  RATING_UNRATED_MAX = 0;

  RATING_ONE_STAR_MIN = 1;
  RATING_ONE_STAR_SET = 1;
  RATING_ONE_STAR_MAX = 12;

  RATING_TWO_STARS_MIN = 13;
  RATING_TWO_STARS_SET = 25;
  RATING_TWO_STARS_MAX = 37;

  RATING_THREE_STARS_MIN = 38;
  RATING_THREE_STARS_SET = 50;
  RATING_THREE_STARS_MAX = 62;

  RATING_FOUR_STARS_MIN = 63;
  RATING_FOUR_STARS_SET = 75;
  RATING_FOUR_STARS_MAX = 87;

  RATING_FIVE_STARS_MIN = 88;
  RATING_FIVE_STARS_SET = 99;
  RATING_FIVE_STARS_MAX = 99;


//  Name:     System.RatingText -- PKEY_RatingText
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 90197CA7-FD8F-4E8C-9DA3-B57E1E609295, 100
//  
//  This is the user-friendly form of System.Rating.  Not intended to be parsed 
//  programmatically.
  PKEY_RatingText: TPropertyKey = (
      fmtid: (D1: $90197CA7; D2: $FD8F; D3: $4E8C;
      D4: ($9D, $A3, $B5, $7E, $1E, $60, $92, $95));
      pid: 100);

//  Name:     System.Sensitivity -- PKEY_Sensitivity
//  Type:     UInt16 -- VT_UI2
//  FormatID: F8D3F6AC-4874-42CB-BE59-AB454B30716A, 100
//
//  
  PKEY_Sensitivity: TPropertyKey = (
      fmtid: (D1: $F8D3F6AC; D2: $4874; D3: $42CB;
      D4: ($BE, $59, $AB, $45, $4B, $30, $71, $6A));
      pid: 100);

// Possible discrete values for PKEY_Sensitivity are:
  SENSITIVITY_PROP_NORMAL = 0;
  SENSITIVITY_PROP_PERSONAL = 1;
  SENSITIVITY_PROP_PRIVATE = 2;
  SENSITIVITY_PROP_CONFIDENTIAL = 3;

//  Name:     System.SensitivityText -- PKEY_SensitivityText
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: D0C7F054-3F72-4725-8527-129A577CB269, 100
//  
//  This is the user-friendly form of System.Sensitivity.  Not intended to be parsed 
//  programmatically.
  PKEY_SensitivityText: TPropertyKey = (
      fmtid: (D1: $D0C7F054; D2: $3F72; D3: $4725;
      D4: ($85, $27, $12, $9A, $57, $7C, $B2, $69));
      pid: 100);

//  Name:     System.SFGAOFlags -- PKEY_SFGAOFlags
//  Type:     UInt32 -- VT_UI4
//  FormatID: (FMTID_ShellDetails) 28636AA6-953D-11D2-B5D6-00C04FD918D0, 25
//
//  IShellFolder::GetAttributesOf flags, with SFGAO_PKEYSFGAOMASK attributes masked out.
  PKEY_SFGAOFlags: TPropertyKey = (
      fmtid: (D1: $28636AA6; D2: $953D; D3: $11D2;
      D4: ($B5, $D6, $00, $C0, $4F, $D9, $18, $D0));
      pid: 25);

//  Name:     System.SharedWith -- PKEY_SharedWith
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: EF884C5B-2BFE-41BB-AAE5-76EEDF4F9902, 200
//
//  Who is the item shared with?
  PKEY_SharedWith: TPropertyKey = (
      fmtid: (D1: $EF884C5B; D2: $2BFE; D3: $41BB;
      D4: ($AA, $E5, $76, $EE, $DF, $4F, $99, $02));
      pid: 200);

//  Name:     System.ShareUserRating -- PKEY_ShareUserRating
//  Type:     UInt32 -- VT_UI4
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 12 (PIDMSI_SHARE_USER_RATING)
//
//  
  PKEY_ShareUserRating: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 12);

//  Name:     System.Shell.OmitFromView -- PKEY_Shell_OmitFromView
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: DE35258C-C695-4CBC-B982-38B0AD24CED0, 2
//  
//  Set this to a string value of 'True' to omit this item from shell views
  PKEY_Shell_OmitFromView: TPropertyKey = (
      fmtid: (D1: $DE35258C; D2: $C695; D3: $4CBC;
      D4: ($B9, $82, $38, $B0, $AD, $24, $CE, $D0));
      pid: 2);

//  Name:     System.SimpleRating -- PKEY_SimpleRating
//  Type:     UInt32 -- VT_UI4
//  FormatID: A09F084E-AD41-489F-8076-AA5BE3082BCA, 100
//  
//  Indicates the users preference rating of an item on a scale of 0-5 (0=unrated, 1=One Star, 2=Two Stars, 3=Three Stars,
//  4=Four Stars, 5=Five Stars)
  PKEY_SimpleRating: TPropertyKey = (
      fmtid: (D1: $A09F084E; D2: $AD41; D3: $489F;
      D4: ($80, $76, $AA, $5B, $E3, $08, $2B, $CA));
      pid: 100);

//  Name:     System.Size -- PKEY_Size
//  Type:     UInt64 -- VT_UI8
//  FormatID: (FMTID_Storage) B725F130-47EF-101A-A5F1-02608C9EEBAC, 12 (PID_STG_SIZE)
//
//  
  PKEY_Size: TPropertyKey = (
      fmtid: (D1: $B725F130; D2: $47EF; D3: $101A;
      D4: ($A5, $F1, $02, $60, $8C, $9E, $EB, $AC));
      pid: 12);

//  Name:     System.SoftwareUsed -- PKEY_SoftwareUsed
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_ImageProperties) 14B81DA1-0135-4D31-96D9-6CBFC9671A99, 305
//
//  PropertyTagSoftwareUsed
  PKEY_SoftwareUsed: TPropertyKey = (
      fmtid: (D1: $14B81DA1; D2: $0135; D3: $4D31;
      D4: ($96, $D9, $6C, $BF, $C9, $67, $1A, $99));
      pid: 305);

//  Name:     System.SourceItem -- PKEY_SourceItem
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 668CDFA5-7A1B-4323-AE4B-E527393A1D81, 100
  PKEY_SourceItem: TPropertyKey = (
      fmtid: (D1: $668CDFA5; D2: $7A1B; D3: $4323;
      D4: ($AE, $4B, $E5, $27, $39, $3A, $1D, $81));
      pid: 100);

//  Name:     System.StartDate -- PKEY_StartDate
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: 48FD6EC8-8A12-4CDF-A03E-4EC5A511EDDE, 100
  PKEY_StartDate: TPropertyKey = (
      fmtid: (D1: $48FD6EC8; D2: $8A12; D3: $4CDF;
      D4: ($A0, $3E, $4E, $C5, $A5, $11, $ED, $DE));
      pid: 100);

//  Name:     System.Status -- PKEY_Status
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_IntSite) 000214A1-0000-0000-C000-000000000046, 9
  PKEY_Status: TPropertyKey = (
      fmtid: (D1: $000214A1; D2: $0000; D3: $0000;
      D4: ($C0, $00, $00, $00, $00, $00, $00, $46));
      pid: 9);

//  Name:     System.Subject -- PKEY_Subject
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_SummaryInformation) F29F85E0-4FF9-1068-AB91-08002B27B3D9, 3 (PIDSI_SUBJECT)
//
//  
  PKEY_Subject: TPropertyKey = (
      fmtid: (D1: $F29F85E0; D2: $4FF9; D3: $1068;
      D4: ($AB, $91, $08, $00, $2B, $27, $B3, $D9));
      pid: 3);

//  Name:     System.Thumbnail -- PKEY_Thumbnail
//  Type:     Clipboard -- VT_CF
//  FormatID: (FMTID_SummaryInformation) F29F85E0-4FF9-1068-AB91-08002B27B3D9, 17 (PIDSI_THUMBNAIL)
//
//  A data that represents the thumbnail in VT_CF format.
  PKEY_Thumbnail: TPropertyKey = (
      fmtid: (D1: $F29F85E0; D2: $4FF9; D3: $1068;
      D4: ($AB, $91, $08, $00, $2B, $27, $B3, $D9));
      pid: 17);

//  Name:     System.ThumbnailCacheId -- PKEY_ThumbnailCacheId
//  Type:     UInt64 -- VT_UI8
//  FormatID: 446D16B1-8DAD-4870-A748-402EA43D788C, 100
//  
//  Unique value that can be used as a key to cache thumbnails. The value changes when the name, volume, or data modified 
//  of an item changes.
  PKEY_ThumbnailCacheId: TPropertyKey = (
      fmtid: (D1: $446D16B1; D2: $8DAD; D3: $4870;
      D4: ($A7, $48, $40, $2E, $A4, $3D, $78, $8C));
      pid: 100);

//  Name:     System.ThumbnailStream -- PKEY_ThumbnailStream
//  Type:     Stream -- VT_STREAM
//  FormatID: (FMTID_SummaryInformation) F29F85E0-4FF9-1068-AB91-08002B27B3D9, 27
//
//  Data that represents the thumbnail in VT_STREAM format that GDI+/WindowsCodecs supports (jpg, png, etc).
  PKEY_ThumbnailStream: TPropertyKey = (
      fmtid: (D1: $F29F85E0; D2: $4FF9; D3: $1068;
      D4: ($AB, $91, $08, $00, $2B, $27, $B3, $D9));
      pid: 27);

//  Name:     System.Title -- PKEY_Title
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)  Legacy code may treat this as VT_LPSTR.
//  FormatID: (FMTID_SummaryInformation) F29F85E0-4FF9-1068-AB91-08002B27B3D9, 2 (PIDSI_TITLE)
//
//  Title of item.
  PKEY_Title: TPropertyKey = (
      fmtid: (D1: $F29F85E0; D2: $4FF9; D3: $1068;
      D4: ($AB, $91, $08, $00, $2B, $27, $B3, $D9));
      pid: 2);

//  Name:     System.TotalFileSize -- PKEY_TotalFileSize
//  Type:     UInt64 -- VT_UI8
//  FormatID: (FMTID_ShellDetails) 28636AA6-953D-11D2-B5D6-00C04FD918D0, 14
//
//  
  PKEY_TotalFileSize: TPropertyKey = (
      fmtid: (D1: $28636AA6; D2: $953D; D3: $11D2;
      D4: ($B5, $D6, $00, $C0, $4F, $D9, $18, $D0));
      pid: 14);

//  Name:     System.Trademarks -- PKEY_Trademarks
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSFMTID_VERSION) 0CEF7D53-FA64-11D1-A203-0000F81FEDEE, 9 (PIDVSI_Trademarks)
//
//  
  PKEY_Trademarks: TPropertyKey = (
      fmtid: (D1: $0CEF7D53; D2: $FA64; D3: $11D1;
      D4: ($A2, $03, $00, $00, $F8, $1F, $ED, $EE));
      pid: 9);
 
//-----------------------------------------------------------------------------
// Document properties



//  Name:     System.Document.ByteCount -- PKEY_Document_ByteCount
//  Type:     Int32 -- VT_I4
//  FormatID: (FMTID_DocumentSummaryInformation) D5CDD502-2E9C-101B-9397-08002B2CF9AE, 4 (PIDDSI_BYTECOUNT)
//
//  
  PKEY_Document_ByteCount: TPropertyKey = (
      fmtid: (D1: $D5CDD502; D2: $2E9C; D3: $101B;
      D4: ($93, $97, $08, $00, $2B, $2C, $F9, $AE));
      pid: 4);

//  Name:     System.Document.CharacterCount -- PKEY_Document_CharacterCount
//  Type:     Int32 -- VT_I4
//  FormatID: (FMTID_SummaryInformation) F29F85E0-4FF9-1068-AB91-08002B27B3D9, 16 (PIDSI_CHARCOUNT)
//
//  
  PKEY_Document_CharacterCount: TPropertyKey = (
      fmtid: (D1: $F29F85E0; D2: $4FF9; D3: $1068;
      D4: ($AB, $91, $08, $00, $2B, $27, $B3, $D9));
      pid: 16);

//  Name:     System.Document.ClientID -- PKEY_Document_ClientID
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 276D7BB0-5B34-4FB0-AA4B-158ED12A1809, 100
  PKEY_Document_ClientID: TPropertyKey = (
      fmtid: (D1: $276D7BB0; D2: $5B34; D3: $4FB0;
      D4: ($AA, $4B, $15, $8E, $D1, $2A, $18, $09));
      pid: 100);

//  Name:     System.Document.Contributor -- PKEY_Document_Contributor
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: F334115E-DA1B-4509-9B3D-119504DC7ABB, 100
  PKEY_Document_Contributor: TPropertyKey = (
      fmtid: (D1: $F334115E; D2: $DA1B; D3: $4509;
      D4: ($9B, $3D, $11, $95, $04, $DC, $7A, $BB));
      pid: 100);

//  Name:     System.Document.DateCreated -- PKEY_Document_DateCreated
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: (FMTID_SummaryInformation) F29F85E0-4FF9-1068-AB91-08002B27B3D9, 12 (PIDSI_CREATE_DTM)
//  
//  This property is stored in the document, not obtained from the file system.
  PKEY_Document_DateCreated: TPropertyKey = (
      fmtid: (D1: $F29F85E0; D2: $4FF9; D3: $1068;
      D4: ($AB, $91, $08, $00, $2B, $27, $B3, $D9));
      pid: 12);

//  Name:     System.Document.DatePrinted -- PKEY_Document_DatePrinted
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: (FMTID_SummaryInformation) F29F85E0-4FF9-1068-AB91-08002B27B3D9, 11 (PIDSI_LASTPRINTED)
//
//  Legacy name: "DocLastPrinted".
  PKEY_Document_DatePrinted: TPropertyKey = (
      fmtid: (D1: $F29F85E0; D2: $4FF9; D3: $1068;
      D4: ($AB, $91, $08, $00, $2B, $27, $B3, $D9));
      pid: 11);

//  Name:     System.Document.DateSaved -- PKEY_Document_DateSaved
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: (FMTID_SummaryInformation) F29F85E0-4FF9-1068-AB91-08002B27B3D9, 13 (PIDSI_LASTSAVE_DTM)
//
//  Legacy name: "DocLastSavedTm".
  PKEY_Document_DateSaved: TPropertyKey = (
      fmtid: (D1: $F29F85E0; D2: $4FF9; D3: $1068;
      D4: ($AB, $91, $08, $00, $2B, $27, $B3, $D9));
      pid: 13);

//  Name:     System.Document.Division -- PKEY_Document_Division
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 1E005EE6-BF27-428B-B01C-79676ACD2870, 100
  PKEY_Document_Division: TPropertyKey = (
      fmtid: (D1: $1E005EE6; D2: $BF27; D3: $428B;
      D4: ($B0, $1C, $79, $67, $6A, $CD, $28, $70));
      pid: 100);

//  Name:     System.Document.DocumentID -- PKEY_Document_DocumentID
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: E08805C8-E395-40DF-80D2-54F0D6C43154, 100
  PKEY_Document_DocumentID: TPropertyKey = (
      fmtid: (D1: $E08805C8; D2: $E395; D3: $40DF;
      D4: ($80, $D2, $54, $F0, $D6, $C4, $31, $54));
      pid: 100);

//  Name:     System.Document.HiddenSlideCount -- PKEY_Document_HiddenSlideCount
//  Type:     Int32 -- VT_I4
//  FormatID: (FMTID_DocumentSummaryInformation) D5CDD502-2E9C-101B-9397-08002B2CF9AE, 9 (PIDDSI_HIDDENCOUNT)
//
//  
  PKEY_Document_HiddenSlideCount: TPropertyKey = (
      fmtid: (D1: $D5CDD502; D2: $2E9C; D3: $101B;
      D4: ($93, $97, $08, $00, $2B, $2C, $F9, $AE));
      pid: 9);

//  Name:     System.Document.LastAuthor -- PKEY_Document_LastAuthor
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_SummaryInformation) F29F85E0-4FF9-1068-AB91-08002B27B3D9, 8 (PIDSI_LASTAUTHOR)
//
//  
  PKEY_Document_LastAuthor: TPropertyKey = (
      fmtid: (D1: $F29F85E0; D2: $4FF9; D3: $1068;
      D4: ($AB, $91, $08, $00, $2B, $27, $B3, $D9));
      pid: 8);

//  Name:     System.Document.LineCount -- PKEY_Document_LineCount
//  Type:     Int32 -- VT_I4
//  FormatID: (FMTID_DocumentSummaryInformation) D5CDD502-2E9C-101B-9397-08002B2CF9AE, 5 (PIDDSI_LINECOUNT)
//
//  
  PKEY_Document_LineCount: TPropertyKey = (
      fmtid: (D1: $D5CDD502; D2: $2E9C; D3: $101B;
      D4: ($93, $97, $08, $00, $2B, $2C, $F9, $AE));
      pid: 5);

//  Name:     System.Document.Manager -- PKEY_Document_Manager
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_DocumentSummaryInformation) D5CDD502-2E9C-101B-9397-08002B2CF9AE, 14 (PIDDSI_MANAGER)
//
//  
  PKEY_Document_Manager: TPropertyKey = (
      fmtid: (D1: $D5CDD502; D2: $2E9C; D3: $101B;
      D4: ($93, $97, $08, $00, $2B, $2C, $F9, $AE));
      pid: 14);

//  Name:     System.Document.MultimediaClipCount -- PKEY_Document_MultimediaClipCount
//  Type:     Int32 -- VT_I4
//  FormatID: (FMTID_DocumentSummaryInformation) D5CDD502-2E9C-101B-9397-08002B2CF9AE, 10 (PIDDSI_MMCLIPCOUNT)
//
//  
  PKEY_Document_MultimediaClipCount: TPropertyKey = (
      fmtid: (D1: $D5CDD502; D2: $2E9C; D3: $101B;
      D4: ($93, $97, $08, $00, $2B, $2C, $F9, $AE));
      pid: 10);

//  Name:     System.Document.NoteCount -- PKEY_Document_NoteCount
//  Type:     Int32 -- VT_I4
//  FormatID: (FMTID_DocumentSummaryInformation) D5CDD502-2E9C-101B-9397-08002B2CF9AE, 8 (PIDDSI_NOTECOUNT)
//
//  
  PKEY_Document_NoteCount: TPropertyKey = (
      fmtid: (D1: $D5CDD502; D2: $2E9C; D3: $101B;
      D4: ($93, $97, $08, $00, $2B, $2C, $F9, $AE));
      pid: 8);

//  Name:     System.Document.PageCount -- PKEY_Document_PageCount
//  Type:     Int32 -- VT_I4
//  FormatID: (FMTID_SummaryInformation) F29F85E0-4FF9-1068-AB91-08002B27B3D9, 14 (PIDSI_PAGECOUNT)
//
//  
  PKEY_Document_PageCount: TPropertyKey = (
      fmtid: (D1: $F29F85E0; D2: $4FF9; D3: $1068;
      D4: ($AB, $91, $08, $00, $2B, $27, $B3, $D9));
      pid: 14);

//  Name:     System.Document.ParagraphCount -- PKEY_Document_ParagraphCount
//  Type:     Int32 -- VT_I4
//  FormatID: (FMTID_DocumentSummaryInformation) D5CDD502-2E9C-101B-9397-08002B2CF9AE, 6 (PIDDSI_PARCOUNT)
//
//  
  PKEY_Document_ParagraphCount: TPropertyKey = (
      fmtid: (D1: $D5CDD502; D2: $2E9C; D3: $101B;
      D4: ($93, $97, $08, $00, $2B, $2C, $F9, $AE));
      pid: 6);

//  Name:     System.Document.PresentationFormat -- PKEY_Document_PresentationFormat
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_DocumentSummaryInformation) D5CDD502-2E9C-101B-9397-08002B2CF9AE, 3 (PIDDSI_PRESFORMAT)
//
//  
  PKEY_Document_PresentationFormat: TPropertyKey = (
      fmtid: (D1: $D5CDD502; D2: $2E9C; D3: $101B;
      D4: ($93, $97, $08, $00, $2B, $2C, $F9, $AE));
      pid: 3);

//  Name:     System.Document.RevisionNumber -- PKEY_Document_RevisionNumber
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_SummaryInformation) F29F85E0-4FF9-1068-AB91-08002B27B3D9, 9 (PIDSI_REVNUMBER)
//
//  
  PKEY_Document_RevisionNumber: TPropertyKey = (
      fmtid: (D1: $F29F85E0; D2: $4FF9; D3: $1068;
      D4: ($AB, $91, $08, $00, $2B, $27, $B3, $D9));
      pid: 9);

//  Name:     System.Document.Security -- PKEY_Document_Security
//  Type:     Int32 -- VT_I4
//  FormatID: (FMTID_SummaryInformation) F29F85E0-4FF9-1068-AB91-08002B27B3D9, 19
//
//  Access control information, from SummaryInfo propset
  PKEY_Document_Security: TPropertyKey = (
      fmtid: (D1: $F29F85E0; D2: $4FF9; D3: $1068;
      D4: ($AB, $91, $08, $00, $2B, $27, $B3, $D9));
      pid: 19);

//  Name:     System.Document.SlideCount -- PKEY_Document_SlideCount
//  Type:     Int32 -- VT_I4
//  FormatID: (FMTID_DocumentSummaryInformation) D5CDD502-2E9C-101B-9397-08002B2CF9AE, 7 (PIDDSI_SLIDECOUNT)
//
//  
  PKEY_Document_SlideCount: TPropertyKey = (
      fmtid: (D1: $D5CDD502; D2: $2E9C; D3: $101B;
      D4: ($93, $97, $08, $00, $2B, $2C, $F9, $AE));
      pid: 7);

//  Name:     System.Document.Template -- PKEY_Document_Template
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_SummaryInformation) F29F85E0-4FF9-1068-AB91-08002B27B3D9, 7 (PIDSI_TEMPLATE)
//
//  
  PKEY_Document_Template: TPropertyKey = (
      fmtid: (D1: $F29F85E0; D2: $4FF9; D3: $1068;
      D4: ($AB, $91, $08, $00, $2B, $27, $B3, $D9));
      pid: 7);

//  Name:     System.Document.TotalEditingTime -- PKEY_Document_TotalEditingTime
//  Type:     UInt64 -- VT_UI8
//  FormatID: (FMTID_SummaryInformation) F29F85E0-4FF9-1068-AB91-08002B27B3D9, 10 (PIDSI_EDITTIME)
//
//  100ns units, not milliseconds. VT_FILETIME for IPropertySetStorage handlers (legacy)
  PKEY_Document_TotalEditingTime: TPropertyKey = (
      fmtid: (D1: $F29F85E0; D2: $4FF9; D3: $1068;
      D4: ($AB, $91, $08, $00, $2B, $27, $B3, $D9));
      pid: 10);

//  Name:     System.Document.Version -- PKEY_Document_Version
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_DocumentSummaryInformation) D5CDD502-2E9C-101B-9397-08002B2CF9AE, 29
  PKEY_Document_Version: TPropertyKey = (
      fmtid: (D1: $D5CDD502; D2: $2E9C; D3: $101B;
      D4: ($93, $97, $08, $00, $2B, $2C, $F9, $AE));
      pid: 29);

//  Name:     System.Document.WordCount -- PKEY_Document_WordCount
//  Type:     Int32 -- VT_I4
//  FormatID: (FMTID_SummaryInformation) F29F85E0-4FF9-1068-AB91-08002B27B3D9, 15 (PIDSI_WORDCOUNT)
//
//  
  PKEY_Document_WordCount: TPropertyKey = (
      fmtid: (D1: $F29F85E0; D2: $4FF9; D3: $1068;
      D4: ($AB, $91, $08, $00, $2B, $27, $B3, $D9));
      pid: 15);

 
 
//-----------------------------------------------------------------------------
// DRM properties

//  Name:     System.DRM.DatePlayExpires -- PKEY_DRM_DatePlayExpires
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: (FMTID_DRM) AEAC19E4-89AE-4508-B9B7-BB867ABEE2ED, 6 (PIDDRSI_PLAYEXPIRES)
//
//  Indicates when play expires for digital rights management.
  PKEY_DRM_DatePlayExpires: TPropertyKey = (
      fmtid: (D1: $AEAC19E4; D2: $89AE; D3: $4508;
      D4: ($B9, $B7, $BB, $86, $7A, $BE, $E2, $ED));
      pid: 6);

//  Name:     System.DRM.DatePlayStarts -- PKEY_DRM_DatePlayStarts
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: (FMTID_DRM) AEAC19E4-89AE-4508-B9B7-BB867ABEE2ED, 5 (PIDDRSI_PLAYSTARTS)
//
//  Indicates when play starts for digital rights management.
  PKEY_DRM_DatePlayStarts: TPropertyKey = (
      fmtid: (D1: $AEAC19E4; D2: $89AE; D3: $4508;
      D4: ($B9, $B7, $BB, $86, $7A, $BE, $E2, $ED));
      pid: 5);

//  Name:     System.DRM.Description -- PKEY_DRM_Description
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_DRM) AEAC19E4-89AE-4508-B9B7-BB867ABEE2ED, 3 (PIDDRSI_DESCRIPTION)
//
//  Displays the description for digital rights management.
  PKEY_DRM_Description: TPropertyKey = (
      fmtid: (D1: $AEAC19E4; D2: $89AE; D3: $4508;
      D4: ($B9, $B7, $BB, $86, $7A, $BE, $E2, $ED));
      pid: 3);

//  Name:     System.DRM.IsProtected -- PKEY_DRM_IsProtected
//  Type:     Boolean -- VT_BOOL
//  FormatID: (FMTID_DRM) AEAC19E4-89AE-4508-B9B7-BB867ABEE2ED, 2 (PIDDRSI_PROTECTED)
//
//  
  PKEY_DRM_IsProtected: TPropertyKey = (
      fmtid: (D1: $AEAC19E4; D2: $89AE; D3: $4508;
      D4: ($B9, $B7, $BB, $86, $7A, $BE, $E2, $ED));
      pid: 2);

//  Name:     System.DRM.PlayCount -- PKEY_DRM_PlayCount
//  Type:     UInt32 -- VT_UI4
//  FormatID: (FMTID_DRM) AEAC19E4-89AE-4508-B9B7-BB867ABEE2ED, 4 (PIDDRSI_PLAYCOUNT)
//
//  Indicates the play count for digital rights management.
  PKEY_DRM_PlayCount: TPropertyKey = (
      fmtid: (D1: $AEAC19E4; D2: $89AE; D3: $4508;
      D4: ($B9, $B7, $BB, $86, $7A, $BE, $E2, $ED));
      pid: 4);
 
//-----------------------------------------------------------------------------
// GPS properties

//  Name:     System.GPS.Altitude -- PKEY_GPS_Altitude
//  Type:     Double -- VT_R8
//  FormatID: 827EDB4F-5B73-44A7-891D-FDFFABEA35CA, 100
//  
//  Indicates the altitude based on the reference in PKEY_GPS_AltitudeRef.  Calculated from PKEY_GPS_AltitudeNumerator and 
//  PKEY_GPS_AltitudeDenominator
  PKEY_GPS_Altitude: TPropertyKey = (
      fmtid: (D1: $827EDB4F; D2: $5B73; D3: $44A7;
      D4: ($89, $1D, $FD, $FF, $AB, $EA, $35, $CA));
      pid: 100);

//  Name:     System.GPS.AltitudeDenominator -- PKEY_GPS_AltitudeDenominator
//  Type:     UInt32 -- VT_UI4
//  FormatID: 78342DCB-E358-4145-AE9A-6BFE4E0F9F51, 100
//
//  Denominator of PKEY_GPS_Altitude
  PKEY_GPS_AltitudeDenominator: TPropertyKey = (
      fmtid: (D1: $78342DCB; D2: $E358; D3: $4145;
      D4: ($AE, $9A, $6B, $FE, $4E, $0F, $9F, $51));
      pid: 100);

//  Name:     System.GPS.AltitudeNumerator -- PKEY_GPS_AltitudeNumerator
//  Type:     UInt32 -- VT_UI4
//  FormatID: 2DAD1EB7-816D-40D3-9EC3-C9773BE2AADE, 100
//
//  Numerator of PKEY_GPS_Altitude
  PKEY_GPS_AltitudeNumerator: TPropertyKey = (
      fmtid: (D1: $2DAD1EB7; D2: $816D; D3: $40D3;
      D4: ($9E, $C3, $C9, $77, $3B, $E2, $AA, $DE));
      pid: 100);

//  Name:     System.GPS.AltitudeRef -- PKEY_GPS_AltitudeRef
//  Type:     Byte -- VT_UI1
//  FormatID: 46AC629D-75EA-4515-867F-6DC4321C5844, 100
//
//  Indicates the reference for the altitude property. (eg: above sea level, below sea level, absolute value)
  PKEY_GPS_AltitudeRef: TPropertyKey = (
      fmtid: (D1: $46AC629D; D2: $75EA; D3: $4515;
      D4: ($86, $7F, $6D, $C4, $32, $1C, $58, $44));
      pid: 100);

//  Name:     System.GPS.AreaInformation -- PKEY_GPS_AreaInformation
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 972E333E-AC7E-49F1-8ADF-A70D07A9BCAB, 100
//
//  Represents the name of the GPS area
  PKEY_GPS_AreaInformation: TPropertyKey = (
      fmtid: (D1: $972E333E; D2: $AC7E; D3: $49F1;
      D4: ($8A, $DF, $A7, $0D, $07, $A9, $BC, $AB));
      pid: 100);

//  Name:     System.GPS.Date -- PKEY_GPS_Date
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: 3602C812-0F3B-45F0-85AD-603468D69423, 100
//
//  Date and time of the GPS record
  PKEY_GPS_Date: TPropertyKey = (
      fmtid: (D1: $3602C812; D2: $0F3B; D3: $45F0;
      D4: ($85, $AD, $60, $34, $68, $D6, $94, $23));
      pid: 100);

//  Name:     System.GPS.DestBearing -- PKEY_GPS_DestBearing
//  Type:     Double -- VT_R8
//  FormatID: C66D4B3C-E888-47CC-B99F-9DCA3EE34DEA, 100
//  
//  Indicates the bearing to the destination point.  Calculated from PKEY_GPS_DestBearingNumerator and 
//  PKEY_GPS_DestBearingDenominator.
  PKEY_GPS_DestBearing: TPropertyKey = (
      fmtid: (D1: $C66D4B3C; D2: $E888; D3: $47CC;
      D4: ($B9, $9F, $9D, $CA, $3E, $E3, $4D, $EA));
      pid: 100);

//  Name:     System.GPS.DestBearingDenominator -- PKEY_GPS_DestBearingDenominator
//  Type:     UInt32 -- VT_UI4
//  FormatID: 7ABCF4F8-7C3F-4988-AC91-8D2C2E97ECA5, 100
//
//  Denominator of PKEY_GPS_DestBearing
  PKEY_GPS_DestBearingDenominator: TPropertyKey = (
      fmtid: (D1: $7ABCF4F8; D2: $7C3F; D3: $4988;
      D4: ($AC, $91, $8D, $2C, $2E, $97, $EC, $A5));
      pid: 100);

//  Name:     System.GPS.DestBearingNumerator -- PKEY_GPS_DestBearingNumerator
//  Type:     UInt32 -- VT_UI4
//  FormatID: BA3B1DA9-86EE-4B5D-A2A4-A271A429F0CF, 100
//
//  Numerator of PKEY_GPS_DestBearing
  PKEY_GPS_DestBearingNumerator: TPropertyKey = (
      fmtid: (D1: $BA3B1DA9; D2: $86EE; D3: $4B5D;
      D4: ($A2, $A4, $A2, $71, $A4, $29, $F0, $CF));
      pid: 100);

//  Name:     System.GPS.DestBearingRef -- PKEY_GPS_DestBearingRef
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 9AB84393-2A0F-4B75-BB22-7279786977CB, 100
//
//  Indicates the reference used for the giving the bearing to the destination point.  (eg: true direction, magnetic direction)
  PKEY_GPS_DestBearingRef: TPropertyKey = (
      fmtid: (D1: $9AB84393; D2: $2A0F; D3: $4B75;
      D4: ($BB, $22, $72, $79, $78, $69, $77, $CB));
      pid: 100);

//  Name:     System.GPS.DestDistance -- PKEY_GPS_DestDistance
//  Type:     Double -- VT_R8
//  FormatID: A93EAE04-6804-4F24-AC81-09B266452118, 100
//  
//  Indicates the distance to the destination point.  Calculated from PKEY_GPS_DestDistanceNumerator and 
//  PKEY_GPS_DestDistanceDenominator.
  PKEY_GPS_DestDistance: TPropertyKey = (
      fmtid: (D1: $A93EAE04; D2: $6804; D3: $4F24;
      D4: ($AC, $81, $09, $B2, $66, $45, $21, $18));
      pid: 100);

//  Name:     System.GPS.DestDistanceDenominator -- PKEY_GPS_DestDistanceDenominator
//  Type:     UInt32 -- VT_UI4
//  FormatID: 9BC2C99B-AC71-4127-9D1C-2596D0D7DCB7, 100
//
//  Denominator of PKEY_GPS_DestDistance
  PKEY_GPS_DestDistanceDenominator: TPropertyKey = (
      fmtid: (D1: $9BC2C99B; D2: $AC71; D3: $4127;
      D4: ($9D, $1C, $25, $96, $D0, $D7, $DC, $B7));
      pid: 100);

//  Name:     System.GPS.DestDistanceNumerator -- PKEY_GPS_DestDistanceNumerator
//  Type:     UInt32 -- VT_UI4
//  FormatID: 2BDA47DA-08C6-4FE1-80BC-A72FC517C5D0, 100
//
//  Numerator of PKEY_GPS_DestDistance
  PKEY_GPS_DestDistanceNumerator: TPropertyKey = (
      fmtid: (D1: $2BDA47DA; D2: $08C6; D3: $4FE1;
      D4: ($80, $BC, $A7, $2F, $C5, $17, $C5, $D0));
      pid: 100);

//  Name:     System.GPS.DestDistanceRef -- PKEY_GPS_DestDistanceRef
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: ED4DF2D3-8695-450B-856F-F5C1C53ACB66, 100
//
//  Indicates the unit used to express the distance to the destination.  (eg: kilometers, miles, knots)
  PKEY_GPS_DestDistanceRef: TPropertyKey = (
      fmtid: (D1: $ED4DF2D3; D2: $8695; D3: $450B;
      D4: ($85, $6F, $F5, $C1, $C5, $3A, $CB, $66));
      pid: 100);

//  Name:     System.GPS.DestLatitude -- PKEY_GPS_DestLatitude
//  Type:     Multivalue Double -- VT_VECTOR | VT_R8  (For variants: VT_ARRAY | VT_R8)
//  FormatID: 9D1D7CC5-5C39-451C-86B3-928E2D18CC47, 100
//  
//  Indicates the latitude of the destination point.  This is an array of three values.  Index 0 is the degrees, index 1 
//  is the minutes, index 2 is the seconds.  Each is calculated from the values in PKEY_GPS_DestLatitudeNumerator and 
//  PKEY_GPS_DestLatitudeDenominator.
  PKEY_GPS_DestLatitude: TPropertyKey = (
      fmtid: (D1: $9D1D7CC5; D2: $5C39; D3: $451C;
      D4: ($86, $B3, $92, $8E, $2D, $18, $CC, $47));
      pid: 100);

//  Name:     System.GPS.DestLatitudeDenominator -- PKEY_GPS_DestLatitudeDenominator
//  Type:     Multivalue UInt32 -- VT_VECTOR | VT_UI4  (For variants: VT_ARRAY | VT_UI4)
//  FormatID: 3A372292-7FCA-49A7-99D5-E47BB2D4E7AB, 100
//
//  Denominator of PKEY_GPS_DestLatitude
  PKEY_GPS_DestLatitudeDenominator: TPropertyKey = (
      fmtid: (D1: $3A372292; D2: $7FCA; D3: $49A7;
      D4: ($99, $D5, $E4, $7B, $B2, $D4, $E7, $AB));
      pid: 100);

//  Name:     System.GPS.DestLatitudeNumerator -- PKEY_GPS_DestLatitudeNumerator
//  Type:     Multivalue UInt32 -- VT_VECTOR | VT_UI4  (For variants: VT_ARRAY | VT_UI4)
//  FormatID: ECF4B6F6-D5A6-433C-BB92-4076650FC890, 100
//
//  Numerator of PKEY_GPS_DestLatitude
  PKEY_GPS_DestLatitudeNumerator: TPropertyKey = (
      fmtid: (D1: $ECF4B6F6; D2: $D5A6; D3: $433C;
      D4: ($BB, $92, $40, $76, $65, $0F, $C8, $90));
      pid: 100);

//  Name:     System.GPS.DestLatitudeRef -- PKEY_GPS_DestLatitudeRef
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: CEA820B9-CE61-4885-A128-005D9087C192, 100
//
//  Indicates whether the latitude destination point is north or south latitude
  PKEY_GPS_DestLatitudeRef: TPropertyKey = (
      fmtid: (D1: $CEA820B9; D2: $CE61; D3: $4885;
      D4: ($A1, $28, $00, $5D, $90, $87, $C1, $92));
      pid: 100);

//  Name:     System.GPS.DestLongitude -- PKEY_GPS_DestLongitude
//  Type:     Multivalue Double -- VT_VECTOR | VT_R8  (For variants: VT_ARRAY | VT_R8)
//  FormatID: 47A96261-CB4C-4807-8AD3-40B9D9DBC6BC, 100
//  
//  Indicates the latitude of the destination point.  This is an array of three values.  Index 0 is the degrees, index 1 
//  is the minutes, index 2 is the seconds.  Each is calculated from the values in PKEY_GPS_DestLongitudeNumerator and 
//  PKEY_GPS_DestLongitudeDenominator.
  PKEY_GPS_DestLongitude: TPropertyKey = (
      fmtid: (D1: $47A96261; D2: $CB4C; D3: $4807;
      D4: ($8A, $D3, $40, $B9, $D9, $DB, $C6, $BC));
      pid: 100);

//  Name:     System.GPS.DestLongitudeDenominator -- PKEY_GPS_DestLongitudeDenominator
//  Type:     Multivalue UInt32 -- VT_VECTOR | VT_UI4  (For variants: VT_ARRAY | VT_UI4)
//  FormatID: 425D69E5-48AD-4900-8D80-6EB6B8D0AC86, 100
//
//  Denominator of PKEY_GPS_DestLongitude
  PKEY_GPS_DestLongitudeDenominator: TPropertyKey = (
      fmtid: (D1: $425D69E5; D2: $48AD; D3: $4900;
      D4: ($8D, $80, $6E, $B6, $B8, $D0, $AC, $86));
      pid: 100);

//  Name:     System.GPS.DestLongitudeNumerator -- PKEY_GPS_DestLongitudeNumerator
//  Type:     Multivalue UInt32 -- VT_VECTOR | VT_UI4  (For variants: VT_ARRAY | VT_UI4)
//  FormatID: A3250282-FB6D-48D5-9A89-DBCACE75CCCF, 100
//
//  Numerator of PKEY_GPS_DestLongitude
  PKEY_GPS_DestLongitudeNumerator: TPropertyKey = (
      fmtid: (D1: $A3250282; D2: $FB6D; D3: $48D5;
      D4: ($9A, $89, $DB, $CA, $CE, $75, $CC, $CF));
      pid: 100);

//  Name:     System.GPS.DestLongitudeRef -- PKEY_GPS_DestLongitudeRef
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 182C1EA6-7C1C-4083-AB4B-AC6C9F4ED128, 100
//
//  Indicates whether the longitude destination point is east or west longitude
  PKEY_GPS_DestLongitudeRef: TPropertyKey = (
      fmtid: (D1: $182C1EA6; D2: $7C1C; D3: $4083;
      D4: ($AB, $4B, $AC, $6C, $9F, $4E, $D1, $28));
      pid: 100);

//  Name:     System.GPS.Differential -- PKEY_GPS_Differential
//  Type:     UInt16 -- VT_UI2
//  FormatID: AAF4EE25-BD3B-4DD7-BFC4-47F77BB00F6D, 100
//
//  Indicates whether differential correction was applied to the GPS receiver
  PKEY_GPS_Differential: TPropertyKey = (
      fmtid: (D1: $AAF4EE25; D2: $BD3B; D3: $4DD7;
      D4: ($BF, $C4, $47, $F7, $7B, $B0, $0F, $6D));
      pid: 100);

//  Name:     System.GPS.DOP -- PKEY_GPS_DOP
//  Type:     Double -- VT_R8
//  FormatID: 0CF8FB02-1837-42F1-A697-A7017AA289B9, 100
//
//  Indicates the GPS DOP (data degree of precision).  Calculated from PKEY_GPS_DOPNumerator and PKEY_GPS_DOPDenominator
  PKEY_GPS_DOP: TPropertyKey = (
      fmtid: (D1: $0CF8FB02; D2: $1837; D3: $42F1;
      D4: ($A6, $97, $A7, $01, $7A, $A2, $89, $B9));
      pid: 100);

//  Name:     System.GPS.DOPDenominator -- PKEY_GPS_DOPDenominator
//  Type:     UInt32 -- VT_UI4
//  FormatID: A0BE94C5-50BA-487B-BD35-0654BE8881ED, 100
//
//  Denominator of PKEY_GPS_DOP
  PKEY_GPS_DOPDenominator: TPropertyKey = (
      fmtid: (D1: $A0BE94C5; D2: $50BA; D3: $487B;
      D4: ($BD, $35, $06, $54, $BE, $88, $81, $ED));
      pid: 100);

//  Name:     System.GPS.DOPNumerator -- PKEY_GPS_DOPNumerator
//  Type:     UInt32 -- VT_UI4
//  FormatID: 47166B16-364F-4AA0-9F31-E2AB3DF449C3, 100
//
//  Numerator of PKEY_GPS_DOP
  PKEY_GPS_DOPNumerator: TPropertyKey = (
      fmtid: (D1: $47166B16; D2: $364F; D3: $4AA0;
      D4: ($9F, $31, $E2, $AB, $3D, $F4, $49, $C3));
      pid: 100);

//  Name:     System.GPS.ImgDirection -- PKEY_GPS_ImgDirection
//  Type:     Double -- VT_R8
//  FormatID: 16473C91-D017-4ED9-BA4D-B6BAA55DBCF8, 100
//  
//  Indicates direction of the image when it was captured.  Calculated from PKEY_GPS_ImgDirectionNumerator and 
//  PKEY_GPS_ImgDirectionDenominator.
  PKEY_GPS_ImgDirection: TPropertyKey = (
      fmtid: (D1: $16473C91; D2: $D017; D3: $4ED9;
      D4: ($BA, $4D, $B6, $BA, $A5, $5D, $BC, $F8));
      pid: 100);

//  Name:     System.GPS.ImgDirectionDenominator -- PKEY_GPS_ImgDirectionDenominator
//  Type:     UInt32 -- VT_UI4
//  FormatID: 10B24595-41A2-4E20-93C2-5761C1395F32, 100
//
//  Denominator of PKEY_GPS_ImgDirection
  PKEY_GPS_ImgDirectionDenominator: TPropertyKey = (
      fmtid: (D1: $10B24595; D2: $41A2; D3: $4E20;
      D4: ($93, $C2, $57, $61, $C1, $39, $5F, $32));
      pid: 100);

//  Name:     System.GPS.ImgDirectionNumerator -- PKEY_GPS_ImgDirectionNumerator
//  Type:     UInt32 -- VT_UI4
//  FormatID: DC5877C7-225F-45F7-BAC7-E81334B6130A, 100
//
//  Numerator of PKEY_GPS_ImgDirection
  PKEY_GPS_ImgDirectionNumerator: TPropertyKey = (
      fmtid: (D1: $DC5877C7; D2: $225F; D3: $45F7;
      D4: ($BA, $C7, $E8, $13, $34, $B6, $13, $0A));
      pid: 100);

//  Name:     System.GPS.ImgDirectionRef -- PKEY_GPS_ImgDirectionRef
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: A4AAA5B7-1AD0-445F-811A-0F8F6E67F6B5, 100
//
//  Indicates reference for giving the direction of the image when it was captured.  (eg: true direction, magnetic direction)
  PKEY_GPS_ImgDirectionRef: TPropertyKey = (
      fmtid: (D1: $A4AAA5B7; D2: $1AD0; D3: $445F;
      D4: ($81, $1A, $0F, $8F, $6E, $67, $F6, $B5));
      pid: 100);

//  Name:     System.GPS.Latitude -- PKEY_GPS_Latitude
//  Type:     Multivalue Double -- VT_VECTOR | VT_R8  (For variants: VT_ARRAY | VT_R8)
//  FormatID: 8727CFFF-4868-4EC6-AD5B-81B98521D1AB, 100
//  
//  Indicates the latitude.  This is an array of three values.  Index 0 is the degrees, index 1 is the minutes, index 2 
//  is the seconds.  Each is calculated from the values in PKEY_GPS_LatitudeNumerator and PKEY_GPS_LatitudeDenominator.
  PKEY_GPS_Latitude: TPropertyKey = (
      fmtid: (D1: $8727CFFF; D2: $4868; D3: $4EC6;
      D4: ($AD, $5B, $81, $B9, $85, $21, $D1, $AB));
      pid: 100);

//  Name:     System.GPS.LatitudeDenominator -- PKEY_GPS_LatitudeDenominator
//  Type:     Multivalue UInt32 -- VT_VECTOR | VT_UI4  (For variants: VT_ARRAY | VT_UI4)
//  FormatID: 16E634EE-2BFF-497B-BD8A-4341AD39EEB9, 100
//
//  Denominator of PKEY_GPS_Latitude
  PKEY_GPS_LatitudeDenominator: TPropertyKey = (
      fmtid: (D1: $16E634EE; D2: $2BFF; D3: $497B;
      D4: ($BD, $8A, $43, $41, $AD, $39, $EE, $B9));
      pid: 100);

//  Name:     System.GPS.LatitudeNumerator -- PKEY_GPS_LatitudeNumerator
//  Type:     Multivalue UInt32 -- VT_VECTOR | VT_UI4  (For variants: VT_ARRAY | VT_UI4)
//  FormatID: 7DDAAAD1-CCC8-41AE-B750-B2CB8031AEA2, 100
//
//  Numerator of PKEY_GPS_Latitude
  PKEY_GPS_LatitudeNumerator: TPropertyKey = (
      fmtid: (D1: $7DDAAAD1; D2: $CCC8; D3: $41AE;
      D4: ($B7, $50, $B2, $CB, $80, $31, $AE, $A2));
      pid: 100);

//  Name:     System.GPS.LatitudeRef -- PKEY_GPS_LatitudeRef
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 029C0252-5B86-46C7-ACA0-2769FFC8E3D4, 100
//
//  Indicates whether latitude is north or south latitude 
  PKEY_GPS_LatitudeRef: TPropertyKey = (
      fmtid: (D1: $029C0252; D2: $5B86; D3: $46C7;
      D4: ($AC, $A0, $27, $69, $FF, $C8, $E3, $D4));
      pid: 100);

//  Name:     System.GPS.Longitude -- PKEY_GPS_Longitude
//  Type:     Multivalue Double -- VT_VECTOR | VT_R8  (For variants: VT_ARRAY | VT_R8)
//  FormatID: C4C4DBB2-B593-466B-BBDA-D03D27D5E43A, 100
//  
//  Indicates the longitude.  This is an array of three values.  Index 0 is the degrees, index 1 is the minutes, index 2 
//  is the seconds.  Each is calculated from the values in PKEY_GPS_LongitudeNumerator and PKEY_GPS_LongitudeDenominator.
  PKEY_GPS_Longitude: TPropertyKey = (
      fmtid: (D1: $C4C4DBB2; D2: $B593; D3: $466B;
      D4: ($BB, $DA, $D0, $3D, $27, $D5, $E4, $3A));
      pid: 100);

//  Name:     System.GPS.LongitudeDenominator -- PKEY_GPS_LongitudeDenominator
//  Type:     Multivalue UInt32 -- VT_VECTOR | VT_UI4  (For variants: VT_ARRAY | VT_UI4)
//  FormatID: BE6E176C-4534-4D2C-ACE5-31DEDAC1606B, 100
//
//  Denominator of PKEY_GPS_Longitude
  PKEY_GPS_LongitudeDenominator: TPropertyKey = (
      fmtid: (D1: $BE6E176C; D2: $4534; D3: $4D2C;
      D4: ($AC, $E5, $31, $DE, $DA, $C1, $60, $6B));
      pid: 100);

//  Name:     System.GPS.LongitudeNumerator -- PKEY_GPS_LongitudeNumerator
//  Type:     Multivalue UInt32 -- VT_VECTOR | VT_UI4  (For variants: VT_ARRAY | VT_UI4)
//  FormatID: 02B0F689-A914-4E45-821D-1DDA452ED2C4, 100
//
//  Numerator of PKEY_GPS_Longitude
  PKEY_GPS_LongitudeNumerator: TPropertyKey = (
      fmtid: (D1: $02B0F689; D2: $A914; D3: $4E45;
      D4: ($82, $1D, $1D, $DA, $45, $2E, $D2, $C4));
      pid: 100);

//  Name:     System.GPS.LongitudeRef -- PKEY_GPS_LongitudeRef
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 33DCF22B-28D5-464C-8035-1EE9EFD25278, 100
//
//  Indicates whether longitude is east or west longitude
  PKEY_GPS_LongitudeRef: TPropertyKey = (
      fmtid: (D1: $33DCF22B; D2: $28D5; D3: $464C;
      D4: ($80, $35, $1E, $E9, $EF, $D2, $52, $78));
      pid: 100);

//  Name:     System.GPS.MapDatum -- PKEY_GPS_MapDatum
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 2CA2DAE6-EDDC-407D-BEF1-773942ABFA95, 100
//
//  Indicates the geodetic survey data used by the GPS receiver
  PKEY_GPS_MapDatum: TPropertyKey = (
      fmtid: (D1: $2CA2DAE6; D2: $EDDC; D3: $407D;
      D4: ($BE, $F1, $77, $39, $42, $AB, $FA, $95));
      pid: 100);

//  Name:     System.GPS.MeasureMode -- PKEY_GPS_MeasureMode
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: A015ED5D-AAEA-4D58-8A86-3C586920EA0B, 100
//
//  Indicates the GPS measurement mode.  (eg: 2-dimensional, 3-dimensional)
  PKEY_GPS_MeasureMode: TPropertyKey = (
      fmtid: (D1: $A015ED5D; D2: $AAEA; D3: $4D58;
      D4: ($8A, $86, $3C, $58, $69, $20, $EA, $0B));
      pid: 100);

//  Name:     System.GPS.ProcessingMethod -- PKEY_GPS_ProcessingMethod
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 59D49E61-840F-4AA9-A939-E2099B7F6399, 100
//
//  Indicates the name of the method used for location finding
  PKEY_GPS_ProcessingMethod: TPropertyKey = (
      fmtid: (D1: $59D49E61; D2: $840F; D3: $4AA9;
      D4: ($A9, $39, $E2, $09, $9B, $7F, $63, $99));
      pid: 100);

//  Name:     System.GPS.Satellites -- PKEY_GPS_Satellites
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 467EE575-1F25-4557-AD4E-B8B58B0D9C15, 100
//
//  Indicates the GPS satellites used for measurements
  PKEY_GPS_Satellites: TPropertyKey = (
      fmtid: (D1: $467EE575; D2: $1F25; D3: $4557;
      D4: ($AD, $4E, $B8, $B5, $8B, $0D, $9C, $15));
      pid: 100);

//  Name:     System.GPS.Speed -- PKEY_GPS_Speed
//  Type:     Double -- VT_R8
//  FormatID: DA5D0862-6E76-4E1B-BABD-70021BD25494, 100
//  
//  Indicates the speed of the GPS receiver movement.  Calculated from PKEY_GPS_SpeedNumerator and 
//  PKEY_GPS_SpeedDenominator.
  PKEY_GPS_Speed: TPropertyKey = (
      fmtid: (D1: $DA5D0862; D2: $6E76; D3: $4E1B;
      D4: ($BA, $BD, $70, $02, $1B, $D2, $54, $94));
      pid: 100);

//  Name:     System.GPS.SpeedDenominator -- PKEY_GPS_SpeedDenominator
//  Type:     UInt32 -- VT_UI4
//  FormatID: 7D122D5A-AE5E-4335-8841-D71E7CE72F53, 100
//
//  Denominator of PKEY_GPS_Speed
  PKEY_GPS_SpeedDenominator: TPropertyKey = (
      fmtid: (D1: $7D122D5A; D2: $AE5E; D3: $4335;
      D4: ($88, $41, $D7, $1E, $7C, $E7, $2F, $53));
      pid: 100);

//  Name:     System.GPS.SpeedNumerator -- PKEY_GPS_SpeedNumerator
//  Type:     UInt32 -- VT_UI4
//  FormatID: ACC9CE3D-C213-4942-8B48-6D0820F21C6D, 100
//
//  Numerator of PKEY_GPS_Speed
  PKEY_GPS_SpeedNumerator: TPropertyKey = (
      fmtid: (D1: $ACC9CE3D; D2: $C213; D3: $4942;
      D4: ($8B, $48, $6D, $08, $20, $F2, $1C, $6D));
      pid: 100);

//  Name:     System.GPS.SpeedRef -- PKEY_GPS_SpeedRef
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: ECF7F4C9-544F-4D6D-9D98-8AD79ADAF453, 100
//  
//  Indicates the unit used to express the speed of the GPS receiver movement.  (eg: kilometers per hour, 
//  miles per hour, knots).
  PKEY_GPS_SpeedRef: TPropertyKey = (
      fmtid: (D1: $ECF7F4C9; D2: $544F; D3: $4D6D;
      D4: ($9D, $98, $8A, $D7, $9A, $DA, $F4, $53));
      pid: 100);

//  Name:     System.GPS.Status -- PKEY_GPS_Status
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 125491F4-818F-46B2-91B5-D537753617B2, 100
//  
//  Indicates the status of the GPS receiver when the image was recorded.  (eg: measurement in progress, 
//  measurement interoperability).
  PKEY_GPS_Status: TPropertyKey = (
      fmtid: (D1: $125491F4; D2: $818F; D3: $46B2;
      D4: ($91, $B5, $D5, $37, $75, $36, $17, $B2));
      pid: 100);

//  Name:     System.GPS.Track -- PKEY_GPS_Track
//  Type:     Double -- VT_R8
//  FormatID: 76C09943-7C33-49E3-9E7E-CDBA872CFADA, 100
//  
//  Indicates the direction of the GPS receiver movement.  Calculated from PKEY_GPS_TrackNumerator and 
//  PKEY_GPS_TrackDenominator.
  PKEY_GPS_Track: TPropertyKey = (
      fmtid: (D1: $76C09943; D2: $7C33; D3: $49E3;
      D4: ($9E, $7E, $CD, $BA, $87, $2C, $FA, $DA));
      pid: 100);

//  Name:     System.GPS.TrackDenominator -- PKEY_GPS_TrackDenominator
//  Type:     UInt32 -- VT_UI4
//  FormatID: C8D1920C-01F6-40C0-AC86-2F3A4AD00770, 100
//
//  Denominator of PKEY_GPS_Track
  PKEY_GPS_TrackDenominator: TPropertyKey = (
      fmtid: (D1: $C8D1920C; D2: $01F6; D3: $40C0;
      D4: ($AC, $86, $2F, $3A, $4A, $D0, $07, $70));
      pid: 100);

//  Name:     System.GPS.TrackNumerator -- PKEY_GPS_TrackNumerator
//  Type:     UInt32 -- VT_UI4
//  FormatID: 702926F4-44A6-43E1-AE71-45627116893B, 100
//
//  Numerator of PKEY_GPS_Track
  PKEY_GPS_TrackNumerator: TPropertyKey = (
      fmtid: (D1: $702926F4; D2: $44A6; D3: $43E1;
      D4: ($AE, $71, $45, $62, $71, $16, $89, $3B));
      pid: 100);

//  Name:     System.GPS.TrackRef -- PKEY_GPS_TrackRef
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 35DBE6FE-44C3-4400-AAAE-D2C799C407E8, 100
//
//  Indicates reference for the direction of the GPS receiver movement.  (eg: true direction, magnetic direction)
  PKEY_GPS_TrackRef: TPropertyKey = (
      fmtid: (D1: $35DBE6FE; D2: $44C3; D3: $4400;
      D4: ($AA, $AE, $D2, $C7, $99, $C4, $07, $E8));
      pid: 100);

//  Name:     System.GPS.VersionID -- PKEY_GPS_VersionID
//  Type:     Buffer -- VT_VECTOR | VT_UI1  (For variants: VT_ARRAY | VT_UI1)
//  FormatID: 22704DA4-C6B2-4A99-8E56-F16DF8C92599, 100
//
//  Indicates the version of the GPS information
  PKEY_GPS_VersionID: TPropertyKey = (
      fmtid: (D1: $22704DA4; D2: $C6B2; D3: $4A99;
      D4: ($8E, $56, $F1, $6D, $F8, $C9, $25, $99));
      pid: 100);
 
//-----------------------------------------------------------------------------
// Image properties



//  Name:     System.Image.BitDepth -- PKEY_Image_BitDepth
//  Type:     UInt32 -- VT_UI4
//  FormatID: (PSGUID_IMAGESUMMARYINFORMATION) 6444048F-4C8B-11D1-8B70-080036B11A03, 7 (PIDISI_BITDEPTH)
//
//  
  PKEY_Image_BitDepth: TPropertyKey = (
      fmtid: (D1: $6444048F; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 7);

//  Name:     System.Image.ColorSpace -- PKEY_Image_ColorSpace
//  Type:     UInt16 -- VT_UI2
//  FormatID: (FMTID_ImageProperties) 14B81DA1-0135-4D31-96D9-6CBFC9671A99, 40961
//
//  PropertyTagExifColorSpace
  PKEY_Image_ColorSpace: TPropertyKey = (
      fmtid: (D1: $14B81DA1; D2: $0135; D3: $4D31;
      D4: ($96, $D9, $6C, $BF, $C9, $67, $1A, $99));
      pid: 40961);

// Possible discrete values for PKEY_Image_ColorSpace are:
  IMAGE_COLORSPACE_SRGB = 1;
  IMAGE_COLORSPACE_UNCALIBRATED = $FFFF;

//  Name:     System.Image.CompressedBitsPerPixel -- PKEY_Image_CompressedBitsPerPixel
//  Type:     Double -- VT_R8
//  FormatID: 364B6FA9-37AB-482A-BE2B-AE02F60D4318, 100
//
//  Calculated from PKEY_Image_CompressedBitsPerPixelNumerator and PKEY_Image_CompressedBitsPerPixelDenominator.
  PKEY_Image_CompressedBitsPerPixel: TPropertyKey = (
      fmtid: (D1: $364B6FA9; D2: $37AB; D3: $482A;
      D4: ($BE, $2B, $AE, $02, $F6, $0D, $43, $18));
      pid: 100);

//  Name:     System.Image.CompressedBitsPerPixelDenominator -- PKEY_Image_CompressedBitsPerPixelDenominator
//  Type:     UInt32 -- VT_UI4
//  FormatID: 1F8844E1-24AD-4508-9DFD-5326A415CE02, 100
//
//  Denominator of PKEY_Image_CompressedBitsPerPixel.
  PKEY_Image_CompressedBitsPerPixelDenominator: TPropertyKey = (
      fmtid: (D1: $1F8844E1; D2: $24AD; D3: $4508;
      D4: ($9D, $FD, $53, $26, $A4, $15, $CE, $02));
      pid: 100);

//  Name:     System.Image.CompressedBitsPerPixelNumerator -- PKEY_Image_CompressedBitsPerPixelNumerator
//  Type:     UInt32 -- VT_UI4
//  FormatID: D21A7148-D32C-4624-8900-277210F79C0F, 100
//
//  Numerator of PKEY_Image_CompressedBitsPerPixel.
  PKEY_Image_CompressedBitsPerPixelNumerator: TPropertyKey = (
      fmtid: (D1: $D21A7148; D2: $D32C; D3: $4624;
      D4: ($89, $00, $27, $72, $10, $F7, $9C, $0F));
      pid: 100);

//  Name:     System.Image.Compression -- PKEY_Image_Compression
//  Type:     UInt16 -- VT_UI2
//  FormatID: (FMTID_ImageProperties) 14B81DA1-0135-4D31-96D9-6CBFC9671A99, 259
//
//  Indicates the image compression level.  PropertyTagCompression.
  PKEY_Image_Compression: TPropertyKey = (
      fmtid: (D1: $14B81DA1; D2: $0135; D3: $4D31;
      D4: ($96, $D9, $6C, $BF, $C9, $67, $1A, $99));
      pid: 259);

// Possible discrete values for PKEY_Image_Compression are:
  IMAGE_COMPRESSION_UNCOMPRESSED = 1;
  IMAGE_COMPRESSION_CCITT_T3 = 2;
  IMAGE_COMPRESSION_CCITT_T4 = 3;
  IMAGE_COMPRESSION_CCITT_T6 = 4;
  IMAGE_COMPRESSION_LZW = 5;
  IMAGE_COMPRESSION_JPEG = 6;
  IMAGE_COMPRESSION_PACKBITS = 32773;

//  Name:     System.Image.CompressionText -- PKEY_Image_CompressionText
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 3F08E66F-2F44-4BB9-A682-AC35D2562322, 100
//  
//  This is the user-friendly form of System.Image.Compression.  Not intended to be parsed 
//  programmatically.
  PKEY_Image_CompressionText: TPropertyKey = (
      fmtid: (D1: $3F08E66F; D2: $2F44; D3: $4BB9;
      D4: ($A6, $82, $AC, $35, $D2, $56, $23, $22));
      pid: 100);

//  Name:     System.Image.Dimensions -- PKEY_Image_Dimensions
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSGUID_IMAGESUMMARYINFORMATION) 6444048F-4C8B-11D1-8B70-080036B11A03, 13 (PIDISI_DIMENSIONS)
//
//  Indicates the dimensions of the image.
  PKEY_Image_Dimensions: TPropertyKey = (
      fmtid: (D1: $6444048F; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 13);

//  Name:     System.Image.HorizontalResolution -- PKEY_Image_HorizontalResolution
//  Type:     Double -- VT_R8
//  FormatID: (PSGUID_IMAGESUMMARYINFORMATION) 6444048F-4C8B-11D1-8B70-080036B11A03, 5 (PIDISI_RESOLUTIONX)
//
//  
  PKEY_Image_HorizontalResolution: TPropertyKey = (
      fmtid: (D1: $6444048F; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 5);

//  Name:     System.Image.HorizontalSize -- PKEY_Image_HorizontalSize
//  Type:     UInt32 -- VT_UI4
//  FormatID: (PSGUID_IMAGESUMMARYINFORMATION) 6444048F-4C8B-11D1-8B70-080036B11A03, 3 (PIDISI_CX)
//
//  
  PKEY_Image_HorizontalSize: TPropertyKey = (
      fmtid: (D1: $6444048F; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 3);

//  Name:     System.Image.ImageID -- PKEY_Image_ImageID
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 10DABE05-32AA-4C29-BF1A-63E2D220587F, 100
  PKEY_Image_ImageID: TPropertyKey = (
      fmtid: (D1: $10DABE05; D2: $32AA; D3: $4C29;
      D4: ($BF, $1A, $63, $E2, $D2, $20, $58, $7F));
      pid: 100);

//  Name:     System.Image.ResolutionUnit -- PKEY_Image_ResolutionUnit
//  Type:     Int16 -- VT_I2
//  FormatID: 19B51FA6-1F92-4A5C-AB48-7DF0ABD67444, 100
  PKEY_Image_ResolutionUnit: TPropertyKey = (
      fmtid: (D1: $19B51FA6; D2: $1F92; D3: $4A5C;
      D4: ($AB, $48, $7D, $F0, $AB, $D6, $74, $44));
      pid: 100);

//  Name:     System.Image.VerticalResolution -- PKEY_Image_VerticalResolution
//  Type:     Double -- VT_R8
//  FormatID: (PSGUID_IMAGESUMMARYINFORMATION) 6444048F-4C8B-11D1-8B70-080036B11A03, 6 (PIDISI_RESOLUTIONY)
//
//  
  PKEY_Image_VerticalResolution: TPropertyKey = (
      fmtid: (D1: $6444048F; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 6);

//  Name:     System.Image.VerticalSize -- PKEY_Image_VerticalSize
//  Type:     UInt32 -- VT_UI4
//  FormatID: (PSGUID_IMAGESUMMARYINFORMATION) 6444048F-4C8B-11D1-8B70-080036B11A03, 4 (PIDISI_CY)
//
//  
  PKEY_Image_VerticalSize: TPropertyKey = (
      fmtid: (D1: $6444048F; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 4);

 
 
//-----------------------------------------------------------------------------
// Journal properties

//  Name:     System.Journal.Contacts -- PKEY_Journal_Contacts
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: DEA7C82C-1D89-4A66-9427-A4E3DEBABCB1, 100
  PKEY_Journal_Contacts: TPropertyKey = (
      fmtid: (D1: $DEA7C82C; D2: $1D89; D3: $4A66;
      D4: ($94, $27, $A4, $E3, $DE, $BA, $BC, $B1));
      pid: 100);

//  Name:     System.Journal.EntryType -- PKEY_Journal_EntryType
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 95BEB1FC-326D-4644-B396-CD3ED90E6DDF, 100
  PKEY_Journal_EntryType: TPropertyKey = (
      fmtid: (D1: $95BEB1FC; D2: $326D; D3: $4644;
      D4: ($B3, $96, $CD, $3E, $D9, $0E, $6D, $DF));
      pid: 100);
 
//-----------------------------------------------------------------------------
// Link properties



//  Name:     System.Link.Comment -- PKEY_Link_Comment
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSGUID_LINK) B9B4B3FC-2B51-4A42-B5D8-324146AFCF25, 5
  PKEY_Link_Comment: TPropertyKey = (
      fmtid: (D1: $B9B4B3FC; D2: $2B51; D3: $4A42;
      D4: ($B5, $D8, $32, $41, $46, $AF, $CF, $25));
      pid: 5);

//  Name:     System.Link.DateVisited -- PKEY_Link_DateVisited
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: 5CBF2787-48CF-4208-B90E-EE5E5D420294, 23  (PKEYs relating to URLs.  Used by IE History.)
  PKEY_Link_DateVisited: TPropertyKey = (
      fmtid: (D1: $5CBF2787; D2: $48CF; D3: $4208;
      D4: ($B9, $0E, $EE, $5E, $5D, $42, $02, $94));
      pid: 23);

//  Name:     System.Link.Description -- PKEY_Link_Description
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 5CBF2787-48CF-4208-B90E-EE5E5D420294, 21  (PKEYs relating to URLs.  Used by IE History.)
  PKEY_Link_Description: TPropertyKey = (
      fmtid: (D1: $5CBF2787; D2: $48CF; D3: $4208;
      D4: ($B9, $0E, $EE, $5E, $5D, $42, $02, $94));
      pid: 21);

//  Name:     System.Link.Status -- PKEY_Link_Status
//  Type:     Int32 -- VT_I4
//  FormatID: (PSGUID_LINK) B9B4B3FC-2B51-4A42-B5D8-324146AFCF25, 3 (PID_LINK_TARGET_TYPE)
//
//  
  PKEY_Link_Status: TPropertyKey = (
      fmtid: (D1: $B9B4B3FC; D2: $2B51; D3: $4A42;
      D4: ($B5, $D8, $32, $41, $46, $AF, $CF, $25));
      pid: 3);

// Possible discrete values for PKEY_Link_Status are:
  LINK_STATUS_RESOLVED = 1;
  LINK_STATUS_BROKEN = 2;

//  Name:     System.Link.TargetExtension -- PKEY_Link_TargetExtension
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: 7A7D76F4-B630-4BD7-95FF-37CC51A975C9, 2
//
//  The file extension of the link target.  See System.File.Extension
  PKEY_Link_TargetExtension: TPropertyKey = (
      fmtid: (D1: $7A7D76F4; D2: $B630; D3: $4BD7;
      D4: ($95, $FF, $37, $CC, $51, $A9, $75, $C9));
      pid: 2);

//  Name:     System.Link.TargetParsingPath -- PKEY_Link_TargetParsingPath
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSGUID_LINK) B9B4B3FC-2B51-4A42-B5D8-324146AFCF25, 2 (PID_LINK_TARGET)
//  
//  This is the shell namespace path to the target of the link item.  This path may be passed to 
//  SHParseDisplayName to parse the path to the correct shell folder.
//  
//  If the target item is a file, the value is identical to System.ItemPathDisplay.
//  
//  If the target item cannot be accessed through the shell namespace, this value is VT_EMPTY.
  PKEY_Link_TargetParsingPath: TPropertyKey = (
      fmtid: (D1: $B9B4B3FC; D2: $2B51; D3: $4A42;
      D4: ($B5, $D8, $32, $41, $46, $AF, $CF, $25));
      pid: 2);

//  Name:     System.Link.TargetSFGAOFlags -- PKEY_Link_TargetSFGAOFlags
//  Type:     UInt32 -- VT_UI4
//  FormatID: (PSGUID_LINK) B9B4B3FC-2B51-4A42-B5D8-324146AFCF25, 8
//  
//  IShellFolder::GetAttributesOf flags for the target of a link, with SFGAO_PKEYSFGAOMASK 
//  attributes masked out.
  PKEY_Link_TargetSFGAOFlags: TPropertyKey = (
      fmtid: (D1: $B9B4B3FC; D2: $2B51; D3: $4A42;
      D4: ($B5, $D8, $32, $41, $46, $AF, $CF, $25));
      pid: 8);
 
//-----------------------------------------------------------------------------
// Media properties



//  Name:     System.Media.AuthorUrl -- PKEY_Media_AuthorUrl
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 32 (PIDMSI_AUTHOR_URL)
//
//  
  PKEY_Media_AuthorUrl: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 32);

//  Name:     System.Media.AverageLevel -- PKEY_Media_AverageLevel
//  Type:     UInt32 -- VT_UI4
//  FormatID: 09EDD5B6-B301-43C5-9990-D00302EFFD46, 100
  PKEY_Media_AverageLevel: TPropertyKey = (
      fmtid: (D1: $09EDD5B6; D2: $B301; D3: $43C5;
      D4: ($99, $90, $D0, $03, $02, $EF, $FD, $46));
      pid: 100);

//  Name:     System.Media.ClassPrimaryID -- PKEY_Media_ClassPrimaryID
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 13 (PIDMSI_CLASS_PRIMARY_ID)
//
//  
  PKEY_Media_ClassPrimaryID: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 13);

//  Name:     System.Media.ClassSecondaryID -- PKEY_Media_ClassSecondaryID
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 14 (PIDMSI_CLASS_SECONDARY_ID)
//
//  
  PKEY_Media_ClassSecondaryID: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 14);

//  Name:     System.Media.CollectionGroupID -- PKEY_Media_CollectionGroupID
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 24 (PIDMSI_COLLECTION_GROUP_ID)
//
//  
  PKEY_Media_CollectionGroupID: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 24);

//  Name:     System.Media.CollectionID -- PKEY_Media_CollectionID
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 25 (PIDMSI_COLLECTION_ID)
//
//  
  PKEY_Media_CollectionID: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 25);

//  Name:     System.Media.ContentDistributor -- PKEY_Media_ContentDistributor
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 18 (PIDMSI_CONTENTDISTRIBUTOR)
//
//  
  PKEY_Media_ContentDistributor: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 18);

//  Name:     System.Media.ContentID -- PKEY_Media_ContentID
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 26 (PIDMSI_CONTENT_ID)
//
//  
  PKEY_Media_ContentID: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 26);

//  Name:     System.Media.CreatorApplication -- PKEY_Media_CreatorApplication
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 27 (PIDMSI_TOOL_NAME)
//
//  
  PKEY_Media_CreatorApplication: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 27);

//  Name:     System.Media.CreatorApplicationVersion -- PKEY_Media_CreatorApplicationVersion
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 28 (PIDMSI_TOOL_VERSION)
//
//  
  PKEY_Media_CreatorApplicationVersion: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 28);

//  Name:     System.Media.DateEncoded -- PKEY_Media_DateEncoded
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: 2E4B640D-5019-46D8-8881-55414CC5CAA0, 100
//
//  DateTime is in UTC (in the doc, not file system).
  PKEY_Media_DateEncoded: TPropertyKey = (
      fmtid: (D1: $2E4B640D; D2: $5019; D3: $46D8;
      D4: ($88, $81, $55, $41, $4C, $C5, $CA, $A0));
      pid: 100);

//  Name:     System.Media.DateReleased -- PKEY_Media_DateReleased
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: DE41CC29-6971-4290-B472-F59F2E2F31E2, 100
  PKEY_Media_DateReleased: TPropertyKey = (
      fmtid: (D1: $DE41CC29; D2: $6971; D3: $4290;
      D4: ($B4, $72, $F5, $9F, $2E, $2F, $31, $E2));
      pid: 100);

//  Name:     System.Media.Duration -- PKEY_Media_Duration
//  Type:     UInt64 -- VT_UI8
//  FormatID: (FMTID_AudioSummaryInformation) 64440490-4C8B-11D1-8B70-080036B11A03, 3 (PIDASI_TIMELENGTH)
//
//  100ns units, not milliseconds
  PKEY_Media_Duration: TPropertyKey = (
      fmtid: (D1: $64440490; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 3);

//  Name:     System.Media.DVDID -- PKEY_Media_DVDID
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 15 (PIDMSI_DVDID)
//
//  
  PKEY_Media_DVDID: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 15);

//  Name:     System.Media.EncodedBy -- PKEY_Media_EncodedBy
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 36 (PIDMSI_ENCODED_BY)
//
//  
  PKEY_Media_EncodedBy: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 36);

//  Name:     System.Media.EncodingSettings -- PKEY_Media_EncodingSettings
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 37 (PIDMSI_ENCODING_SETTINGS)
//
//  
  PKEY_Media_EncodingSettings: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 37);

//  Name:     System.Media.FrameCount -- PKEY_Media_FrameCount
//  Type:     UInt32 -- VT_UI4
//  FormatID: (PSGUID_IMAGESUMMARYINFORMATION) 6444048F-4C8B-11D1-8B70-080036B11A03, 12 (PIDISI_FRAMECOUNT)
//
//  Indicates the frame count for the image.
  PKEY_Media_FrameCount: TPropertyKey = (
      fmtid: (D1: $6444048F; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 12);

//  Name:     System.Media.MCDI -- PKEY_Media_MCDI
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 16 (PIDMSI_MCDI)
//
//  
  PKEY_Media_MCDI: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 16);

//  Name:     System.Media.MetadataContentProvider -- PKEY_Media_MetadataContentProvider
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 17 (PIDMSI_PROVIDER)
//
//  
  PKEY_Media_MetadataContentProvider: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 17);

//  Name:     System.Media.Producer -- PKEY_Media_Producer
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 22 (PIDMSI_PRODUCER)
//
//  
  PKEY_Media_Producer: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 22);

//  Name:     System.Media.PromotionUrl -- PKEY_Media_PromotionUrl
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 33 (PIDMSI_PROMOTION_URL)
//
//  
  PKEY_Media_PromotionUrl: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 33);

//  Name:     System.Media.ProtectionType -- PKEY_Media_ProtectionType
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 38
//  
//  If media is protected, how is it protected?
  PKEY_Media_ProtectionType: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 38);

//  Name:     System.Media.ProviderRating -- PKEY_Media_ProviderRating
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 39
//  
//  Rating (0 - 99) supplied by metadata provider
  PKEY_Media_ProviderRating: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 39);

//  Name:     System.Media.ProviderStyle -- PKEY_Media_ProviderStyle
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 40
//  
//  Style of music or video, supplied by metadata provider
  PKEY_Media_ProviderStyle: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 40);

//  Name:     System.Media.Publisher -- PKEY_Media_Publisher
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 30 (PIDMSI_PUBLISHER)
//
//  
  PKEY_Media_Publisher: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 30);

//  Name:     System.Media.SubscriptionContentId -- PKEY_Media_SubscriptionContentId
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 9AEBAE7A-9644-487D-A92C-657585ED751A, 100
  PKEY_Media_SubscriptionContentId: TPropertyKey = (
      fmtid: (D1: $9AEBAE7A; D2: $9644; D3: $487D;
      D4: ($A9, $2C, $65, $75, $85, $ED, $75, $1A));
      pid: 100);

//  Name:     System.Media.SubTitle -- PKEY_Media_SubTitle
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_MUSIC) 56A3372E-CE9C-11D2-9F0E-006097C686F6, 38 (PIDSI_MUSIC_SUB_TITLE)
//
//  
  PKEY_Media_SubTitle: TPropertyKey = (
      fmtid: (D1: $56A3372E; D2: $CE9C; D3: $11D2;
      D4: ($9F, $0E, $00, $60, $97, $C6, $86, $F6));
      pid: 38);

//  Name:     System.Media.UniqueFileIdentifier -- PKEY_Media_UniqueFileIdentifier
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 35 (PIDMSI_UNIQUE_FILE_IDENTIFIER)
//
//  
  PKEY_Media_UniqueFileIdentifier: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 35);

//  Name:     System.Media.UserNoAutoInfo -- PKEY_Media_UserNoAutoInfo
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 41
//  
//  If true, do NOT alter this file's metadata. Set by user.
  PKEY_Media_UserNoAutoInfo: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 41);

//  Name:     System.Media.UserWebUrl -- PKEY_Media_UserWebUrl
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 34 (PIDMSI_USER_WEB_URL)
//
//  
  PKEY_Media_UserWebUrl: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 34);

//  Name:     System.Media.Writer -- PKEY_Media_Writer
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 23 (PIDMSI_WRITER)
//
//  
  PKEY_Media_Writer: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 23);

//  Name:     System.Media.Year -- PKEY_Media_Year
//  Type:     UInt32 -- VT_UI4
//  FormatID: (FMTID_MUSIC) 56A3372E-CE9C-11D2-9F0E-006097C686F6, 5 (PIDSI_MUSIC_YEAR)
//
//  
  PKEY_Media_Year: TPropertyKey = (
      fmtid: (D1: $56A3372E; D2: $CE9C; D3: $11D2;
      D4: ($9F, $0E, $00, $60, $97, $C6, $86, $F6));
      pid: 5);
 
//-----------------------------------------------------------------------------
// Message properties



//  Name:     System.Message.AttachmentContents -- PKEY_Message_AttachmentContents
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 3143BF7C-80A8-4854-8880-E2E40189BDD0, 100
  PKEY_Message_AttachmentContents: TPropertyKey = (
      fmtid: (D1: $3143BF7C; D2: $80A8; D3: $4854;
      D4: ($88, $80, $E2, $E4, $01, $89, $BD, $D0));
      pid: 100);

//  Name:     System.Message.AttachmentNames -- PKEY_Message_AttachmentNames
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: E3E0584C-B788-4A5A-BB20-7F5A44C9ACDD, 21
//
//  The names of the attachments in a message
  PKEY_Message_AttachmentNames: TPropertyKey = (
      fmtid: (D1: $E3E0584C; D2: $B788; D3: $4A5A;
      D4: ($BB, $20, $7F, $5A, $44, $C9, $AC, $DD));
      pid: 21);

//  Name:     System.Message.BccAddress -- PKEY_Message_BccAddress
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: E3E0584C-B788-4A5A-BB20-7F5A44C9ACDD, 2
//
//  Addresses in Bcc: field
  PKEY_Message_BccAddress: TPropertyKey = (
      fmtid: (D1: $E3E0584C; D2: $B788; D3: $4A5A;
      D4: ($BB, $20, $7F, $5A, $44, $C9, $AC, $DD));
      pid: 2);

//  Name:     System.Message.BccName -- PKEY_Message_BccName
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: E3E0584C-B788-4A5A-BB20-7F5A44C9ACDD, 3
//
//  person names in Bcc: field
  PKEY_Message_BccName: TPropertyKey = (
      fmtid: (D1: $E3E0584C; D2: $B788; D3: $4A5A;
      D4: ($BB, $20, $7F, $5A, $44, $C9, $AC, $DD));
      pid: 3);

//  Name:     System.Message.CcAddress -- PKEY_Message_CcAddress
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: E3E0584C-B788-4A5A-BB20-7F5A44C9ACDD, 4
//
//  Addresses in Cc: field
  PKEY_Message_CcAddress: TPropertyKey = (
      fmtid: (D1: $E3E0584C; D2: $B788; D3: $4A5A;
      D4: ($BB, $20, $7F, $5A, $44, $C9, $AC, $DD));
      pid: 4);

//  Name:     System.Message.CcName -- PKEY_Message_CcName
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: E3E0584C-B788-4A5A-BB20-7F5A44C9ACDD, 5
//
//  person names in Cc: field
  PKEY_Message_CcName: TPropertyKey = (
      fmtid: (D1: $E3E0584C; D2: $B788; D3: $4A5A;
      D4: ($BB, $20, $7F, $5A, $44, $C9, $AC, $DD));
      pid: 5);

//  Name:     System.Message.ConversationID -- PKEY_Message_ConversationID
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: DC8F80BD-AF1E-4289-85B6-3DFC1B493992, 100
  PKEY_Message_ConversationID: TPropertyKey = (
      fmtid: (D1: $DC8F80BD; D2: $AF1E; D3: $4289;
      D4: ($85, $B6, $3D, $FC, $1B, $49, $39, $92));
      pid: 100);

//  Name:     System.Message.ConversationIndex -- PKEY_Message_ConversationIndex
//  Type:     Buffer -- VT_VECTOR | VT_UI1  (For variants: VT_ARRAY | VT_UI1)
//  FormatID: DC8F80BD-AF1E-4289-85B6-3DFC1B493992, 101
//  
//  
  PKEY_Message_ConversationIndex: TPropertyKey = (
      fmtid: (D1: $DC8F80BD; D2: $AF1E; D3: $4289;
      D4: ($85, $B6, $3D, $FC, $1B, $49, $39, $92));
      pid: 101);

//  Name:     System.Message.DateReceived -- PKEY_Message_DateReceived
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: E3E0584C-B788-4A5A-BB20-7F5A44C9ACDD, 20
//
//  Date and Time communication was received
  PKEY_Message_DateReceived: TPropertyKey = (
      fmtid: (D1: $E3E0584C; D2: $B788; D3: $4A5A;
      D4: ($BB, $20, $7F, $5A, $44, $C9, $AC, $DD));
      pid: 20);

//  Name:     System.Message.DateSent -- PKEY_Message_DateSent
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: E3E0584C-B788-4A5A-BB20-7F5A44C9ACDD, 19
//
//  Date and Time communication was sent
  PKEY_Message_DateSent: TPropertyKey = (
      fmtid: (D1: $E3E0584C; D2: $B788; D3: $4A5A;
      D4: ($BB, $20, $7F, $5A, $44, $C9, $AC, $DD));
      pid: 19);

//  Name:     System.Message.FromAddress -- PKEY_Message_FromAddress
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: E3E0584C-B788-4A5A-BB20-7F5A44C9ACDD, 13
  PKEY_Message_FromAddress: TPropertyKey = (
      fmtid: (D1: $E3E0584C; D2: $B788; D3: $4A5A;
      D4: ($BB, $20, $7F, $5A, $44, $C9, $AC, $DD));
      pid: 13);

//  Name:     System.Message.FromName -- PKEY_Message_FromName
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: E3E0584C-B788-4A5A-BB20-7F5A44C9ACDD, 14
//
//  Address in from field as person name
  PKEY_Message_FromName: TPropertyKey = (
      fmtid: (D1: $E3E0584C; D2: $B788; D3: $4A5A;
      D4: ($BB, $20, $7F, $5A, $44, $C9, $AC, $DD));
      pid: 14);

//  Name:     System.Message.HasAttachments -- PKEY_Message_HasAttachments
//  Type:     Boolean -- VT_BOOL
//  FormatID: 9C1FCF74-2D97-41BA-B4AE-CB2E3661A6E4, 8
//
//  
  PKEY_Message_HasAttachments: TPropertyKey = (
      fmtid: (D1: $9C1FCF74; D2: $2D97; D3: $41BA;
      D4: ($B4, $AE, $CB, $2E, $36, $61, $A6, $E4));
      pid: 8);

//  Name:     System.Message.IsFwdOrReply -- PKEY_Message_IsFwdOrReply
//  Type:     Int32 -- VT_I4
//  FormatID: 9A9BC088-4F6D-469E-9919-E705412040F9, 100
  PKEY_Message_IsFwdOrReply: TPropertyKey = (
      fmtid: (D1: $9A9BC088; D2: $4F6D; D3: $469E;
      D4: ($99, $19, $E7, $05, $41, $20, $40, $F9));
      pid: 100);

//  Name:     System.Message.MessageClass -- PKEY_Message_MessageClass
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: CD9ED458-08CE-418F-A70E-F912C7BB9C5C, 103
//  
//  What type of outlook msg this is (meeting, task, mail, etc.)
  PKEY_Message_MessageClass: TPropertyKey = (
      fmtid: (D1: $CD9ED458; D2: $08CE; D3: $418F;
      D4: ($A7, $0E, $F9, $12, $C7, $BB, $9C, $5C));
      pid: 103);

//  Name:     System.Message.SenderAddress -- PKEY_Message_SenderAddress
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 0BE1C8E7-1981-4676-AE14-FDD78F05A6E7, 100
  PKEY_Message_SenderAddress: TPropertyKey = (
      fmtid: (D1: $0BE1C8E7; D2: $1981; D3: $4676;
      D4: ($AE, $14, $FD, $D7, $8F, $05, $A6, $E7));
      pid: 100);

//  Name:     System.Message.SenderName -- PKEY_Message_SenderName
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 0DA41CFA-D224-4A18-AE2F-596158DB4B3A, 100
  PKEY_Message_SenderName: TPropertyKey = (
      fmtid: (D1: $0DA41CFA; D2: $D224; D3: $4A18;
      D4: ($AE, $2F, $59, $61, $58, $DB, $4B, $3A));
      pid: 100);

//  Name:     System.Message.Store -- PKEY_Message_Store
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: E3E0584C-B788-4A5A-BB20-7F5A44C9ACDD, 15
//
//  The store (aka protocol handler) FILE, MAIL, OUTLOOKEXPRESS
  PKEY_Message_Store: TPropertyKey = (
      fmtid: (D1: $E3E0584C; D2: $B788; D3: $4A5A;
      D4: ($BB, $20, $7F, $5A, $44, $C9, $AC, $DD));
      pid: 15);

//  Name:     System.Message.ToAddress -- PKEY_Message_ToAddress
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: E3E0584C-B788-4A5A-BB20-7F5A44C9ACDD, 16
//
//  Addresses in To: field
  PKEY_Message_ToAddress: TPropertyKey = (
      fmtid: (D1: $E3E0584C; D2: $B788; D3: $4A5A;
      D4: ($BB, $20, $7F, $5A, $44, $C9, $AC, $DD));
      pid: 16);

//  Name:     System.Message.ToDoTitle -- PKEY_Message_ToDoTitle
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: BCCC8A3C-8CEF-42E5-9B1C-C69079398BC7, 100
  PKEY_Message_ToDoTitle: TPropertyKey = (
      fmtid: (D1: $BCCC8A3C; D2: $8CEF; D3: $42E5;
      D4: ($9B, $1C, $C6, $90, $79, $39, $8B, $C7));
      pid: 100);

//  Name:     System.Message.ToName -- PKEY_Message_ToName
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: E3E0584C-B788-4A5A-BB20-7F5A44C9ACDD, 17
//
//  Person names in To: field
  PKEY_Message_ToName: TPropertyKey = (
      fmtid: (D1: $E3E0584C; D2: $B788; D3: $4A5A;
      D4: ($BB, $20, $7F, $5A, $44, $C9, $AC, $DD));
      pid: 17);
 
//-----------------------------------------------------------------------------
// Music properties

//  Name:     System.Music.AlbumArtist -- PKEY_Music_AlbumArtist
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_MUSIC) 56A3372E-CE9C-11D2-9F0E-006097C686F6, 13 (PIDSI_MUSIC_ALBUM_ARTIST)
//
//  
  PKEY_Music_AlbumArtist: TPropertyKey = (
      fmtid: (D1: $56A3372E; D2: $CE9C; D3: $11D2;
      D4: ($9F, $0E, $00, $60, $97, $C6, $86, $F6));
      pid: 13);

//  Name:     System.Music.AlbumTitle -- PKEY_Music_AlbumTitle
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_MUSIC) 56A3372E-CE9C-11D2-9F0E-006097C686F6, 4 (PIDSI_MUSIC_ALBUM)
//
//  
  PKEY_Music_AlbumTitle: TPropertyKey = (
      fmtid: (D1: $56A3372E; D2: $CE9C; D3: $11D2;
      D4: ($9F, $0E, $00, $60, $97, $C6, $86, $F6));
      pid: 4);

//  Name:     System.Music.Artist -- PKEY_Music_Artist
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: (FMTID_MUSIC) 56A3372E-CE9C-11D2-9F0E-006097C686F6, 2 (PIDSI_MUSIC_ARTIST)
//
//  
  PKEY_Music_Artist: TPropertyKey = (
      fmtid: (D1: $56A3372E; D2: $CE9C; D3: $11D2;
      D4: ($9F, $0E, $00, $60, $97, $C6, $86, $F6));
      pid: 2);

//  Name:     System.Music.BeatsPerMinute -- PKEY_Music_BeatsPerMinute
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_MUSIC) 56A3372E-CE9C-11D2-9F0E-006097C686F6, 35 (PIDSI_MUSIC_BEATS_PER_MINUTE)
//
//  
  PKEY_Music_BeatsPerMinute: TPropertyKey = (
      fmtid: (D1: $56A3372E; D2: $CE9C; D3: $11D2;
      D4: ($9F, $0E, $00, $60, $97, $C6, $86, $F6));
      pid: 35);

//  Name:     System.Music.Composer -- PKEY_Music_Composer
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 19 (PIDMSI_COMPOSER)
//
//  
  PKEY_Music_Composer: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 19);

//  Name:     System.Music.Conductor -- PKEY_Music_Conductor
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: (FMTID_MUSIC) 56A3372E-CE9C-11D2-9F0E-006097C686F6, 36 (PIDSI_MUSIC_CONDUCTOR)
//
//  
  PKEY_Music_Conductor: TPropertyKey = (
      fmtid: (D1: $56A3372E; D2: $CE9C; D3: $11D2;
      D4: ($9F, $0E, $00, $60, $97, $C6, $86, $F6));
      pid: 36);

//  Name:     System.Music.ContentGroupDescription -- PKEY_Music_ContentGroupDescription
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_MUSIC) 56A3372E-CE9C-11D2-9F0E-006097C686F6, 33 (PIDSI_MUSIC_CONTENT_GROUP_DESCRIPTION)
//
//  
  PKEY_Music_ContentGroupDescription: TPropertyKey = (
      fmtid: (D1: $56A3372E; D2: $CE9C; D3: $11D2;
      D4: ($9F, $0E, $00, $60, $97, $C6, $86, $F6));
      pid: 33);

//  Name:     System.Music.Genre -- PKEY_Music_Genre
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: (FMTID_MUSIC) 56A3372E-CE9C-11D2-9F0E-006097C686F6, 11 (PIDSI_MUSIC_GENRE)
//
//  
  PKEY_Music_Genre: TPropertyKey = (
      fmtid: (D1: $56A3372E; D2: $CE9C; D3: $11D2;
      D4: ($9F, $0E, $00, $60, $97, $C6, $86, $F6));
      pid: 11);

//  Name:     System.Music.InitialKey -- PKEY_Music_InitialKey
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_MUSIC) 56A3372E-CE9C-11D2-9F0E-006097C686F6, 34 (PIDSI_MUSIC_INITIAL_KEY)
//
//  
  PKEY_Music_InitialKey: TPropertyKey = (
      fmtid: (D1: $56A3372E; D2: $CE9C; D3: $11D2;
      D4: ($9F, $0E, $00, $60, $97, $C6, $86, $F6));
      pid: 34);

//  Name:     System.Music.IsCompilation -- PKEY_Music_IsCompilation
//  Type:     Boolean -- VT_BOOL
//  FormatID: {C449D5CB-9EA4-4809-82E8-AF9D59DED6D1}, 100
//
//
  PKEY_Music_IsCompilation: TPropertyKey = (
      fmtid: (D1: $C449D5CB; D2: $9EA4; D3: $4809;
      D4: ($82, $E8, $AF, $9D, $59, $DE, $D6, $D1));
      pid: 100);

//  Name:     System.Music.Lyrics -- PKEY_Music_Lyrics
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_MUSIC) 56A3372E-CE9C-11D2-9F0E-006097C686F6, 12 (PIDSI_MUSIC_LYRICS)
//
//  
  PKEY_Music_Lyrics: TPropertyKey = (
      fmtid: (D1: $56A3372E; D2: $CE9C; D3: $11D2;
      D4: ($9F, $0E, $00, $60, $97, $C6, $86, $F6));
      pid: 12);

//  Name:     System.Music.Mood -- PKEY_Music_Mood
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_MUSIC) 56A3372E-CE9C-11D2-9F0E-006097C686F6, 39 (PIDSI_MUSIC_MOOD)
//
//  
  PKEY_Music_Mood: TPropertyKey = (
      fmtid: (D1: $56A3372E; D2: $CE9C; D3: $11D2;
      D4: ($9F, $0E, $00, $60, $97, $C6, $86, $F6));
      pid: 39);

//  Name:     System.Music.PartOfSet -- PKEY_Music_PartOfSet
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_MUSIC) 56A3372E-CE9C-11D2-9F0E-006097C686F6, 37 (PIDSI_MUSIC_PART_OF_SET)
//
//  
  PKEY_Music_PartOfSet: TPropertyKey = (
      fmtid: (D1: $56A3372E; D2: $CE9C; D3: $11D2;
      D4: ($9F, $0E, $00, $60, $97, $C6, $86, $F6));
      pid: 37);

//  Name:     System.Music.Period -- PKEY_Music_Period
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 31 (PIDMSI_PERIOD)
//
//  
  PKEY_Music_Period: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 31);

//  Name:     System.Music.SynchronizedLyrics -- PKEY_Music_SynchronizedLyrics
//  Type:     Blob -- VT_BLOB
//  FormatID: 6B223B6A-162E-4AA9-B39F-05D678FC6D77, 100
  PKEY_Music_SynchronizedLyrics: TPropertyKey = (
      fmtid: (D1: $6B223B6A; D2: $162E; D3: $4AA9;
      D4: ($B3, $9F, $05, $D6, $78, $FC, $6D, $77));
      pid: 100);

//  Name:     System.Music.TrackNumber -- PKEY_Music_TrackNumber
//  Type:     UInt32 -- VT_UI4
//  FormatID: (FMTID_MUSIC) 56A3372E-CE9C-11D2-9F0E-006097C686F6, 7 (PIDSI_MUSIC_TRACK)
//
//  
  PKEY_Music_TrackNumber: TPropertyKey = (
      fmtid: (D1: $56A3372E; D2: $CE9C; D3: $11D2;
      D4: ($9F, $0E, $00, $60, $97, $C6, $86, $F6));
      pid: 7);

 
 
//-----------------------------------------------------------------------------
// Note properties

//  Name:     System.Note.Color -- PKEY_Note_Color
//  Type:     UInt16 -- VT_UI2
//  FormatID: 4776CAFA-BCE4-4CB1-A23E-265E76D8EB11, 100
  PKEY_Note_Color: TPropertyKey = (
      fmtid: (D1: $4776CAFA; D2: $BCE4; D3: $4CB1;
      D4: ($A2, $3E, $26, $5E, $76, $D8, $EB, $11));
      pid: 100);

// Possible discrete values for PKEY_Note_Color are:
  NOTE_COLOR_BLUE = 0;
  NOTE_COLOR_GREEN = 1;
  NOTE_COLOR_PINK = 2;
  NOTE_COLOR_YELLOW = 3;
  NOTE_COLOR_WHITE = 4;
  NOTE_COLOR_LIGHTGREEN = 5;

//  Name:     System.Note.ColorText -- PKEY_Note_ColorText
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 46B4E8DE-CDB2-440D-885C-1658EB65B914, 100
//  
//  This is the user-friendly form of System.Note.Color.  Not intended to be parsed 
//  programmatically.
  PKEY_Note_ColorText: TPropertyKey = (
      fmtid: (D1: $46B4E8DE; D2: $CDB2; D3: $440D;
      D4: ($88, $5C, $16, $58, $EB, $65, $B9, $14));
      pid: 100);
 
//-----------------------------------------------------------------------------
// Photo properties



//  Name:     System.Photo.Aperture -- PKEY_Photo_Aperture
//  Type:     Double -- VT_R8
//  FormatID: (FMTID_ImageProperties) 14B81DA1-0135-4D31-96D9-6CBFC9671A99, 37378
//
//  PropertyTagExifAperture.  Calculated from PKEY_Photo_ApertureNumerator and PKEY_Photo_ApertureDenominator
  PKEY_Photo_Aperture: TPropertyKey = (
      fmtid: (D1: $14B81DA1; D2: $0135; D3: $4D31;
      D4: ($96, $D9, $6C, $BF, $C9, $67, $1A, $99));
      pid: 37378);

//  Name:     System.Photo.ApertureDenominator -- PKEY_Photo_ApertureDenominator
//  Type:     UInt32 -- VT_UI4
//  FormatID: E1A9A38B-6685-46BD-875E-570DC7AD7320, 100
//
//  Denominator of PKEY_Photo_Aperture
  PKEY_Photo_ApertureDenominator: TPropertyKey = (
      fmtid: (D1: $E1A9A38B; D2: $6685; D3: $46BD;
      D4: ($87, $5E, $57, $0D, $C7, $AD, $73, $20));
      pid: 100);

//  Name:     System.Photo.ApertureNumerator -- PKEY_Photo_ApertureNumerator
//  Type:     UInt32 -- VT_UI4
//  FormatID: 0337ECEC-39FB-4581-A0BD-4C4CC51E9914, 100
//
//  Numerator of PKEY_Photo_Aperture
  PKEY_Photo_ApertureNumerator: TPropertyKey = (
      fmtid: (D1: $0337ECEC; D2: $39FB; D3: $4581;
      D4: ($A0, $BD, $4C, $4C, $C5, $1E, $99, $14));
      pid: 100);

//  Name:     System.Photo.Brightness -- PKEY_Photo_Brightness
//  Type:     Double -- VT_R8
//  FormatID: 1A701BF6-478C-4361-83AB-3701BB053C58, 100 (PropertyTagExifBrightness)
//  
//  This is the brightness of the photo.
//  
//  Calculated from PKEY_Photo_BrightnessNumerator and PKEY_Photo_BrightnessDenominator.
//  
//  The units are "APEX", normally in the range of -99.99 to 99.99. If the numerator of 
//  the recorded value is FFFFFFFF.H, "Unknown" should be indicated.
  PKEY_Photo_Brightness: TPropertyKey = (
      fmtid: (D1: $1A701BF6; D2: $478C; D3: $4361;
      D4: ($83, $AB, $37, $01, $BB, $05, $3C, $58));
      pid: 100);

//  Name:     System.Photo.BrightnessDenominator -- PKEY_Photo_BrightnessDenominator
//  Type:     UInt32 -- VT_UI4
//  FormatID: 6EBE6946-2321-440A-90F0-C043EFD32476, 100
//
//  Denominator of PKEY_Photo_Brightness
  PKEY_Photo_BrightnessDenominator: TPropertyKey = (
      fmtid: (D1: $6EBE6946; D2: $2321; D3: $440A;
      D4: ($90, $F0, $C0, $43, $EF, $D3, $24, $76));
      pid: 100);

//  Name:     System.Photo.BrightnessNumerator -- PKEY_Photo_BrightnessNumerator
//  Type:     UInt32 -- VT_UI4
//  FormatID: 9E7D118F-B314-45A0-8CFB-D654B917C9E9, 100
//
//  Numerator of PKEY_Photo_Brightness
  PKEY_Photo_BrightnessNumerator: TPropertyKey = (
      fmtid: (D1: $9E7D118F; D2: $B314; D3: $45A0;
      D4: ($8C, $FB, $D6, $54, $B9, $17, $C9, $E9));
      pid: 100);

//  Name:     System.Photo.CameraManufacturer -- PKEY_Photo_CameraManufacturer
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_ImageProperties) 14B81DA1-0135-4D31-96D9-6CBFC9671A99, 271 (PropertyTagEquipMake)
//
//  
  PKEY_Photo_CameraManufacturer: TPropertyKey = (
      fmtid: (D1: $14B81DA1; D2: $0135; D3: $4D31;
      D4: ($96, $D9, $6C, $BF, $C9, $67, $1A, $99));
      pid: 271);

//  Name:     System.Photo.CameraModel -- PKEY_Photo_CameraModel
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_ImageProperties) 14B81DA1-0135-4D31-96D9-6CBFC9671A99, 272 (PropertyTagEquipModel)
//
//  
  PKEY_Photo_CameraModel: TPropertyKey = (
      fmtid: (D1: $14B81DA1; D2: $0135; D3: $4D31;
      D4: ($96, $D9, $6C, $BF, $C9, $67, $1A, $99));
      pid: 272);

//  Name:     System.Photo.CameraSerialNumber -- PKEY_Photo_CameraSerialNumber
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_ImageProperties) 14B81DA1-0135-4D31-96D9-6CBFC9671A99, 273
//
//  Serial number of camera that produced this photo
  PKEY_Photo_CameraSerialNumber: TPropertyKey = (
      fmtid: (D1: $14B81DA1; D2: $0135; D3: $4D31;
      D4: ($96, $D9, $6C, $BF, $C9, $67, $1A, $99));
      pid: 273);

//  Name:     System.Photo.Contrast -- PKEY_Photo_Contrast
//  Type:     UInt32 -- VT_UI4
//  FormatID: 2A785BA9-8D23-4DED-82E6-60A350C86A10, 100
//  
//  This indicates the direction of contrast processing applied by the camera 
//  when the image was shot.
  PKEY_Photo_Contrast: TPropertyKey = (
      fmtid: (D1: $2A785BA9; D2: $8D23; D3: $4DED;
      D4: ($82, $E6, $60, $A3, $50, $C8, $6A, $10));
      pid: 100);

// Possible discrete values for PKEY_Photo_Contrast are:
  PHOTO_CONTRAST_NORMAL = 0;
  PHOTO_CONTRAST_SOFT = 1;
  PHOTO_CONTRAST_HARD = 2;

//  Name:     System.Photo.ContrastText -- PKEY_Photo_ContrastText
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 59DDE9F2-5253-40EA-9A8B-479E96C6249A, 100
//  
//  This is the user-friendly form of System.Photo.Contrast.  Not intended to be parsed 
//  programmatically.
  PKEY_Photo_ContrastText: TPropertyKey = (
      fmtid: (D1: $59DDE9F2; D2: $5253; D3: $40EA;
      D4: ($9A, $8B, $47, $9E, $96, $C6, $24, $9A));
      pid: 100);

//  Name:     System.Photo.DateTaken -- PKEY_Photo_DateTaken
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: (FMTID_ImageProperties) 14B81DA1-0135-4D31-96D9-6CBFC9671A99, 36867
//
//  PropertyTagExifDTOrig
  PKEY_Photo_DateTaken: TPropertyKey = (
      fmtid: (D1: $14B81DA1; D2: $0135; D3: $4D31;
      D4: ($96, $D9, $6C, $BF, $C9, $67, $1A, $99));
      pid: 36867);

//  Name:     System.Photo.DigitalZoom -- PKEY_Photo_DigitalZoom
//  Type:     Double -- VT_R8
//  FormatID: F85BF840-A925-4BC2-B0C4-8E36B598679E, 100
//
//  PropertyTagExifDigitalZoom.  Calculated from PKEY_Photo_DigitalZoomNumerator and PKEY_Photo_DigitalZoomDenominator
  PKEY_Photo_DigitalZoom: TPropertyKey = (
      fmtid: (D1: $F85BF840; D2: $A925; D3: $4BC2;
      D4: ($B0, $C4, $8E, $36, $B5, $98, $67, $9E));
      pid: 100);

//  Name:     System.Photo.DigitalZoomDenominator -- PKEY_Photo_DigitalZoomDenominator
//  Type:     UInt32 -- VT_UI4
//  FormatID: 745BAF0E-E5C1-4CFB-8A1B-D031A0A52393, 100
//
//  Denominator of PKEY_Photo_DigitalZoom
  PKEY_Photo_DigitalZoomDenominator: TPropertyKey = (
      fmtid: (D1: $745BAF0E; D2: $E5C1; D3: $4CFB;
      D4: ($8A, $1B, $D0, $31, $A0, $A5, $23, $93));
      pid: 100);

//  Name:     System.Photo.DigitalZoomNumerator -- PKEY_Photo_DigitalZoomNumerator
//  Type:     UInt32 -- VT_UI4
//  FormatID: 16CBB924-6500-473B-A5BE-F1599BCBE413, 100
//
//  Numerator of PKEY_Photo_DigitalZoom
  PKEY_Photo_DigitalZoomNumerator: TPropertyKey = (
      fmtid: (D1: $16CBB924; D2: $6500; D3: $473B;
      D4: ($A5, $BE, $F1, $59, $9B, $CB, $E4, $13));
      pid: 100);

//  Name:     System.Photo.Event -- PKEY_Photo_Event
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: (FMTID_ImageProperties) 14B81DA1-0135-4D31-96D9-6CBFC9671A99, 18248
//
//  The event at which the photo was taken
  PKEY_Photo_Event: TPropertyKey = (
      fmtid: (D1: $14B81DA1; D2: $0135; D3: $4D31;
      D4: ($96, $D9, $6C, $BF, $C9, $67, $1A, $99));
      pid: 18248);

//  Name:     System.Photo.EXIFVersion -- PKEY_Photo_EXIFVersion
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: D35F743A-EB2E-47F2-A286-844132CB1427, 100
//
//  The EXIF version.
  PKEY_Photo_EXIFVersion: TPropertyKey = (
      fmtid: (D1: $D35F743A; D2: $EB2E; D3: $47F2;
      D4: ($A2, $86, $84, $41, $32, $CB, $14, $27));
      pid: 100);

//  Name:     System.Photo.ExposureBias -- PKEY_Photo_ExposureBias
//  Type:     Double -- VT_R8
//  FormatID: (FMTID_ImageProperties) 14B81DA1-0135-4D31-96D9-6CBFC9671A99, 37380
//
//  PropertyTagExifExposureBias.  Calculated from PKEY_Photo_ExposureBiasNumerator and PKEY_Photo_ExposureBiasDenominator
  PKEY_Photo_ExposureBias: TPropertyKey = (
      fmtid: (D1: $14B81DA1; D2: $0135; D3: $4D31;
      D4: ($96, $D9, $6C, $BF, $C9, $67, $1A, $99));
      pid: 37380);

//  Name:     System.Photo.ExposureBiasDenominator -- PKEY_Photo_ExposureBiasDenominator
//  Type:     Int32 -- VT_I4
//  FormatID: AB205E50-04B7-461C-A18C-2F233836E627, 100
//
//  Denominator of PKEY_Photo_ExposureBias
  PKEY_Photo_ExposureBiasDenominator: TPropertyKey = (
      fmtid: (D1: $AB205E50; D2: $04B7; D3: $461C;
      D4: ($A1, $8C, $2F, $23, $38, $36, $E6, $27));
      pid: 100);

//  Name:     System.Photo.ExposureBiasNumerator -- PKEY_Photo_ExposureBiasNumerator
//  Type:     Int32 -- VT_I4
//  FormatID: 738BF284-1D87-420B-92CF-5834BF6EF9ED, 100
//
//  Numerator of PKEY_Photo_ExposureBias
  PKEY_Photo_ExposureBiasNumerator: TPropertyKey = (
      fmtid: (D1: $738BF284; D2: $1D87; D3: $420B;
      D4: ($92, $CF, $58, $34, $BF, $6E, $F9, $ED));
      pid: 100);

//  Name:     System.Photo.ExposureIndex -- PKEY_Photo_ExposureIndex
//  Type:     Double -- VT_R8
//  FormatID: 967B5AF8-995A-46ED-9E11-35B3C5B9782D, 100
//
//  PropertyTagExifExposureIndex.  Calculated from PKEY_Photo_ExposureIndexNumerator and PKEY_Photo_ExposureIndexDenominator
  PKEY_Photo_ExposureIndex: TPropertyKey = (
      fmtid: (D1: $967B5AF8; D2: $995A; D3: $46ED;
      D4: ($9E, $11, $35, $B3, $C5, $B9, $78, $2D));
      pid: 100);

//  Name:     System.Photo.ExposureIndexDenominator -- PKEY_Photo_ExposureIndexDenominator
//  Type:     UInt32 -- VT_UI4
//  FormatID: 93112F89-C28B-492F-8A9D-4BE2062CEE8A, 100
//
//  Denominator of PKEY_Photo_ExposureIndex
  PKEY_Photo_ExposureIndexDenominator: TPropertyKey = (
      fmtid: (D1: $93112F89; D2: $C28B; D3: $492F;
      D4: ($8A, $9D, $4B, $E2, $06, $2C, $EE, $8A));
      pid: 100);

//  Name:     System.Photo.ExposureIndexNumerator -- PKEY_Photo_ExposureIndexNumerator
//  Type:     UInt32 -- VT_UI4
//  FormatID: CDEDCF30-8919-44DF-8F4C-4EB2FFDB8D89, 100
//
//  Numerator of PKEY_Photo_ExposureIndex
  PKEY_Photo_ExposureIndexNumerator: TPropertyKey = (
      fmtid: (D1: $CDEDCF30; D2: $8919; D3: $44DF;
      D4: ($8F, $4C, $4E, $B2, $FF, $DB, $8D, $89));
      pid: 100);

//  Name:     System.Photo.ExposureProgram -- PKEY_Photo_ExposureProgram
//  Type:     UInt32 -- VT_UI4
//  FormatID: (FMTID_ImageProperties) 14B81DA1-0135-4D31-96D9-6CBFC9671A99, 34850 (PropertyTagExifExposureProg)
//
//  
  PKEY_Photo_ExposureProgram: TPropertyKey = (
      fmtid: (D1: $14B81DA1; D2: $0135; D3: $4D31;
      D4: ($96, $D9, $6C, $BF, $C9, $67, $1A, $99));
      pid: 34850);

// Possible discrete values for PKEY_Photo_ExposureProgram are:
  PHOTO_EXPOSUREPROGRAM_UNKNOWN = 0;
  PHOTO_EXPOSUREPROGRAM_MANUAL = 1;
  PHOTO_EXPOSUREPROGRAM_NORMAL = 2;
  PHOTO_EXPOSUREPROGRAM_APERTURE = 3;
  PHOTO_EXPOSUREPROGRAM_SHUTTER = 4;
  PHOTO_EXPOSUREPROGRAM_CREATIVE = 5;
  PHOTO_EXPOSUREPROGRAM_ACTION = 6;
  PHOTO_EXPOSUREPROGRAM_PORTRAIT = 7;
  PHOTO_EXPOSUREPROGRAM_LANDSCAPE = 8;

//  Name:     System.Photo.ExposureProgramText -- PKEY_Photo_ExposureProgramText
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: FEC690B7-5F30-4646-AE47-4CAAFBA884A3, 100
//  
//  This is the user-friendly form of System.Photo.ExposureProgram.  Not intended to be parsed 
//  programmatically.
  PKEY_Photo_ExposureProgramText: TPropertyKey = (
      fmtid: (D1: $FEC690B7; D2: $5F30; D3: $4646;
      D4: ($AE, $47, $4C, $AA, $FB, $A8, $84, $A3));
      pid: 100);

//  Name:     System.Photo.ExposureTime -- PKEY_Photo_ExposureTime
//  Type:     Double -- VT_R8
//  FormatID: (FMTID_ImageProperties) 14B81DA1-0135-4D31-96D9-6CBFC9671A99, 33434
//
//  PropertyTagExifExposureTime.  Calculated from  PKEY_Photo_ExposureTimeNumerator and PKEY_Photo_ExposureTimeDenominator
  PKEY_Photo_ExposureTime: TPropertyKey = (
      fmtid: (D1: $14B81DA1; D2: $0135; D3: $4D31;
      D4: ($96, $D9, $6C, $BF, $C9, $67, $1A, $99));
      pid: 33434);

//  Name:     System.Photo.ExposureTimeDenominator -- PKEY_Photo_ExposureTimeDenominator
//  Type:     UInt32 -- VT_UI4
//  FormatID: 55E98597-AD16-42E0-B624-21599A199838, 100
//
//  Denominator of PKEY_Photo_ExposureTime
  PKEY_Photo_ExposureTimeDenominator: TPropertyKey = (
      fmtid: (D1: $55E98597; D2: $AD16; D3: $42E0;
      D4: ($B6, $24, $21, $59, $9A, $19, $98, $38));
      pid: 100);

//  Name:     System.Photo.ExposureTimeNumerator -- PKEY_Photo_ExposureTimeNumerator
//  Type:     UInt32 -- VT_UI4
//  FormatID: 257E44E2-9031-4323-AC38-85C552871B2E, 100
//
//  Numerator of PKEY_Photo_ExposureTime
  PKEY_Photo_ExposureTimeNumerator: TPropertyKey = (
      fmtid: (D1: $257E44E2; D2: $9031; D3: $4323;
      D4: ($AC, $38, $85, $C5, $52, $87, $1B, $2E));
      pid: 100);

//  Name:     System.Photo.Flash -- PKEY_Photo_Flash
//  Type:     Byte -- VT_UI1
//  FormatID: (FMTID_ImageProperties) 14B81DA1-0135-4D31-96D9-6CBFC9671A99, 37385
//
//  PropertyTagExifFlash
  PKEY_Photo_Flash: TPropertyKey = (
      fmtid: (D1: $14B81DA1; D2: $0135; D3: $4D31;
      D4: ($96, $D9, $6C, $BF, $C9, $67, $1A, $99));
      pid: 37385);

// Possible discrete values for PKEY_Photo_Flash are:
  PHOTO_FLASH_NONE = 0;
  PHOTO_FLASH_FLASH = 1;
  PHOTO_FLASH_WITHOUTSTROBE = 5;
  PHOTO_FLASH_WITHSTROBE = 7;

//  Name:     System.Photo.FlashEnergy -- PKEY_Photo_FlashEnergy
//  Type:     Double -- VT_R8
//  FormatID: (FMTID_ImageProperties) 14B81DA1-0135-4D31-96D9-6CBFC9671A99, 41483
//
//  PropertyTagExifFlashEnergy.  Calculated from PKEY_Photo_FlashEnergyNumerator and PKEY_Photo_FlashEnergyDenominator
  PKEY_Photo_FlashEnergy: TPropertyKey = (
      fmtid: (D1: $14B81DA1; D2: $0135; D3: $4D31;
      D4: ($96, $D9, $6C, $BF, $C9, $67, $1A, $99));
      pid: 41483);

//  Name:     System.Photo.FlashEnergyDenominator -- PKEY_Photo_FlashEnergyDenominator
//  Type:     UInt32 -- VT_UI4
//  FormatID: D7B61C70-6323-49CD-A5FC-C84277162C97, 100
//
//  Denominator of PKEY_Photo_FlashEnergy
  PKEY_Photo_FlashEnergyDenominator: TPropertyKey = (
      fmtid: (D1: $D7B61C70; D2: $6323; D3: $49CD;
      D4: ($A5, $FC, $C8, $42, $77, $16, $2C, $97));
      pid: 100);

//  Name:     System.Photo.FlashEnergyNumerator -- PKEY_Photo_FlashEnergyNumerator
//  Type:     UInt32 -- VT_UI4
//  FormatID: FCAD3D3D-0858-400F-AAA3-2F66CCE2A6BC, 100
//
//  Numerator of PKEY_Photo_FlashEnergy
  PKEY_Photo_FlashEnergyNumerator: TPropertyKey = (
      fmtid: (D1: $FCAD3D3D; D2: $0858; D3: $400F;
      D4: ($AA, $A3, $2F, $66, $CC, $E2, $A6, $BC));
      pid: 100);

//  Name:     System.Photo.FlashManufacturer -- PKEY_Photo_FlashManufacturer
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: AABAF6C9-E0C5-4719-8585-57B103E584FE, 100
  PKEY_Photo_FlashManufacturer: TPropertyKey = (
      fmtid: (D1: $AABAF6C9; D2: $E0C5; D3: $4719;
      D4: ($85, $85, $57, $B1, $03, $E5, $84, $FE));
      pid: 100);

//  Name:     System.Photo.FlashModel -- PKEY_Photo_FlashModel
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: FE83BB35-4D1A-42E2-916B-06F3E1AF719E, 100
  PKEY_Photo_FlashModel: TPropertyKey = (
      fmtid: (D1: $FE83BB35; D2: $4D1A; D3: $42E2;
      D4: ($91, $6B, $06, $F3, $E1, $AF, $71, $9E));
      pid: 100);

//  Name:     System.Photo.FlashText -- PKEY_Photo_FlashText
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 6B8B68F6-200B-47EA-8D25-D8050F57339F, 100
//  
//  This is the user-friendly form of System.Photo.Flash.  Not intended to be parsed 
//  programmatically.
  PKEY_Photo_FlashText: TPropertyKey = (
      fmtid: (D1: $6B8B68F6; D2: $200B; D3: $47EA;
      D4: ($8D, $25, $D8, $05, $0F, $57, $33, $9F));
      pid: 100);

//  Name:     System.Photo.FNumber -- PKEY_Photo_FNumber
//  Type:     Double -- VT_R8
//  FormatID: (FMTID_ImageProperties) 14B81DA1-0135-4D31-96D9-6CBFC9671A99, 33437
//
//  PropertyTagExifFNumber.  Calculated from PKEY_Photo_FNumberNumerator and PKEY_Photo_FNumberDenominator
  PKEY_Photo_FNumber: TPropertyKey = (
      fmtid: (D1: $14B81DA1; D2: $0135; D3: $4D31;
      D4: ($96, $D9, $6C, $BF, $C9, $67, $1A, $99));
      pid: 33437);

//  Name:     System.Photo.FNumberDenominator -- PKEY_Photo_FNumberDenominator
//  Type:     UInt32 -- VT_UI4
//  FormatID: E92A2496-223B-4463-A4E3-30EABBA79D80, 100
//
//  Denominator of PKEY_Photo_FNumber
  PKEY_Photo_FNumberDenominator: TPropertyKey = (
      fmtid: (D1: $E92A2496; D2: $223B; D3: $4463;
      D4: ($A4, $E3, $30, $EA, $BB, $A7, $9D, $80));
      pid: 100);

//  Name:     System.Photo.FNumberNumerator -- PKEY_Photo_FNumberNumerator
//  Type:     UInt32 -- VT_UI4
//  FormatID: 1B97738A-FDFC-462F-9D93-1957E08BE90C, 100
//
//  Numerator of PKEY_Photo_FNumber
  PKEY_Photo_FNumberNumerator: TPropertyKey = (
      fmtid: (D1: $1B97738A; D2: $FDFC; D3: $462F;
      D4: ($9D, $93, $19, $57, $E0, $8B, $E9, $0C));
      pid: 100);

//  Name:     System.Photo.FocalLength -- PKEY_Photo_FocalLength
//  Type:     Double -- VT_R8
//  FormatID: (FMTID_ImageProperties) 14B81DA1-0135-4D31-96D9-6CBFC9671A99, 37386
//
//  PropertyTagExifFocalLength.  Calculated from PKEY_Photo_FocalLengthNumerator and PKEY_Photo_FocalLengthDenominator
  PKEY_Photo_FocalLength: TPropertyKey = (
      fmtid: (D1: $14B81DA1; D2: $0135; D3: $4D31;
      D4: ($96, $D9, $6C, $BF, $C9, $67, $1A, $99));
      pid: 37386);

//  Name:     System.Photo.FocalLengthDenominator -- PKEY_Photo_FocalLengthDenominator
//  Type:     UInt32 -- VT_UI4
//  FormatID: 305BC615-DCA1-44A5-9FD4-10C0BA79412E, 100
//
//  Denominator of PKEY_Photo_FocalLength
  PKEY_Photo_FocalLengthDenominator: TPropertyKey = (
      fmtid: (D1: $305BC615; D2: $DCA1; D3: $44A5;
      D4: ($9F, $D4, $10, $C0, $BA, $79, $41, $2E));
      pid: 100);

//  Name:     System.Photo.FocalLengthInFilm -- PKEY_Photo_FocalLengthInFilm
//  Type:     UInt16 -- VT_UI2
//  FormatID: A0E74609-B84D-4F49-B860-462BD9971F98, 100
  PKEY_Photo_FocalLengthInFilm: TPropertyKey = (
      fmtid: (D1: $A0E74609; D2: $B84D; D3: $4F49;
      D4: ($B8, $60, $46, $2B, $D9, $97, $1F, $98));
      pid: 100);

//  Name:     System.Photo.FocalLengthNumerator -- PKEY_Photo_FocalLengthNumerator
//  Type:     UInt32 -- VT_UI4
//  FormatID: 776B6B3B-1E3D-4B0C-9A0E-8FBAF2A8492A, 100
//
//  Numerator of PKEY_Photo_FocalLength
  PKEY_Photo_FocalLengthNumerator: TPropertyKey = (
      fmtid: (D1: $776B6B3B; D2: $1E3D; D3: $4B0C;
      D4: ($9A, $0E, $8F, $BA, $F2, $A8, $49, $2A));
      pid: 100);

//  Name:     System.Photo.FocalPlaneXResolution -- PKEY_Photo_FocalPlaneXResolution
//  Type:     Double -- VT_R8
//  FormatID: CFC08D97-C6F7-4484-89DD-EBEF4356FE76, 100
//  
//  PropertyTagExifFocalXRes.  Calculated from PKEY_Photo_FocalPlaneXResolutionNumerator and 
//  PKEY_Photo_FocalPlaneXResolutionDenominator.
  PKEY_Photo_FocalPlaneXResolution: TPropertyKey = (
      fmtid: (D1: $CFC08D97; D2: $C6F7; D3: $4484;
      D4: ($89, $DD, $EB, $EF, $43, $56, $FE, $76));
      pid: 100);

//  Name:     System.Photo.FocalPlaneXResolutionDenominator -- PKEY_Photo_FocalPlaneXResolutionDenominator
//  Type:     UInt32 -- VT_UI4
//  FormatID: 0933F3F5-4786-4F46-A8E8-D64DD37FA521, 100
//
//  Denominator of PKEY_Photo_FocalPlaneXResolution
  PKEY_Photo_FocalPlaneXResolutionDenominator: TPropertyKey = (
      fmtid: (D1: $0933F3F5; D2: $4786; D3: $4F46;
      D4: ($A8, $E8, $D6, $4D, $D3, $7F, $A5, $21));
      pid: 100);

//  Name:     System.Photo.FocalPlaneXResolutionNumerator -- PKEY_Photo_FocalPlaneXResolutionNumerator
//  Type:     UInt32 -- VT_UI4
//  FormatID: DCCB10AF-B4E2-4B88-95F9-031B4D5AB490, 100
//
//  Numerator of PKEY_Photo_FocalPlaneXResolution
  PKEY_Photo_FocalPlaneXResolutionNumerator: TPropertyKey = (
      fmtid: (D1: $DCCB10AF; D2: $B4E2; D3: $4B88;
      D4: ($95, $F9, $03, $1B, $4D, $5A, $B4, $90));
      pid: 100);

//  Name:     System.Photo.FocalPlaneYResolution -- PKEY_Photo_FocalPlaneYResolution
//  Type:     Double -- VT_R8
//  FormatID: 4FFFE4D0-914F-4AC4-8D6F-C9C61DE169B1, 100
//  
//  PropertyTagExifFocalYRes.  Calculated from PKEY_Photo_FocalPlaneYResolutionNumerator and 
//  PKEY_Photo_FocalPlaneYResolutionDenominator.
  PKEY_Photo_FocalPlaneYResolution: TPropertyKey = (
      fmtid: (D1: $4FFFE4D0; D2: $914F; D3: $4AC4;
      D4: ($8D, $6F, $C9, $C6, $1D, $E1, $69, $B1));
      pid: 100);

//  Name:     System.Photo.FocalPlaneYResolutionDenominator -- PKEY_Photo_FocalPlaneYResolutionDenominator
//  Type:     UInt32 -- VT_UI4
//  FormatID: 1D6179A6-A876-4031-B013-3347B2B64DC8, 100
//
//  Denominator of PKEY_Photo_FocalPlaneYResolution
  PKEY_Photo_FocalPlaneYResolutionDenominator: TPropertyKey = (
      fmtid: (D1: $1D6179A6; D2: $A876; D3: $4031;
      D4: ($B0, $13, $33, $47, $B2, $B6, $4D, $C8));
      pid: 100);

//  Name:     System.Photo.FocalPlaneYResolutionNumerator -- PKEY_Photo_FocalPlaneYResolutionNumerator
//  Type:     UInt32 -- VT_UI4
//  FormatID: A2E541C5-4440-4BA8-867E-75CFC06828CD, 100
//
//  Numerator of PKEY_Photo_FocalPlaneYResolution
  PKEY_Photo_FocalPlaneYResolutionNumerator: TPropertyKey = (
      fmtid: (D1: $A2E541C5; D2: $4440; D3: $4BA8;
      D4: ($86, $7E, $75, $CF, $C0, $68, $28, $CD));
      pid: 100);

//  Name:     System.Photo.GainControl -- PKEY_Photo_GainControl
//  Type:     Double -- VT_R8
//  FormatID: FA304789-00C7-4D80-904A-1E4DCC7265AA, 100 (PropertyTagExifGainControl)
//  
//  This indicates the degree of overall image gain adjustment.
//  
//  Calculated from PKEY_Photo_GainControlNumerator and PKEY_Photo_GainControlDenominator.
  PKEY_Photo_GainControl: TPropertyKey = (
      fmtid: (D1: $FA304789; D2: $00C7; D3: $4D80;
      D4: ($90, $4A, $1E, $4D, $CC, $72, $65, $AA));
      pid: 100);

// Possible discrete values for PKEY_Photo_GainControl are:
  PHOTO_GAINCONTROL_NONE = 0.0;
  PHOTO_GAINCONTROL_LOWGAINUP = 1.0;
  PHOTO_GAINCONTROL_HIGHGAINUP = 2.0;
  PHOTO_GAINCONTROL_LOWGAINDOWN = 3.0;
  PHOTO_GAINCONTROL_HIGHGAINDOWN = 4.0;

//  Name:     System.Photo.GainControlDenominator -- PKEY_Photo_GainControlDenominator
//  Type:     UInt32 -- VT_UI4
//  FormatID: 42864DFD-9DA4-4F77-BDED-4AAD7B256735, 100
//
//  Denominator of PKEY_Photo_GainControl
  PKEY_Photo_GainControlDenominator: TPropertyKey = (
      fmtid: (D1: $42864DFD; D2: $9DA4; D3: $4F77;
      D4: ($BD, $ED, $4A, $AD, $7B, $25, $67, $35));
      pid: 100);

//  Name:     System.Photo.GainControlNumerator -- PKEY_Photo_GainControlNumerator
//  Type:     UInt32 -- VT_UI4
//  FormatID: 8E8ECF7C-B7B8-4EB8-A63F-0EE715C96F9E, 100
//
//  Numerator of PKEY_Photo_GainControl
  PKEY_Photo_GainControlNumerator: TPropertyKey = (
      fmtid: (D1: $8E8ECF7C; D2: $B7B8; D3: $4EB8;
      D4: ($A6, $3F, $0E, $E7, $15, $C9, $6F, $9E));
      pid: 100);

//  Name:     System.Photo.GainControlText -- PKEY_Photo_GainControlText
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: C06238B2-0BF9-4279-A723-25856715CB9D, 100
//  
//  This is the user-friendly form of System.Photo.GainControl.  Not intended to be parsed 
//  programmatically.
  PKEY_Photo_GainControlText: TPropertyKey = (
      fmtid: (D1: $C06238B2; D2: $0BF9; D3: $4279;
      D4: ($A7, $23, $25, $85, $67, $15, $CB, $9D));
      pid: 100);

//  Name:     System.Photo.ISOSpeed -- PKEY_Photo_ISOSpeed
//  Type:     UInt16 -- VT_UI2
//  FormatID: (FMTID_ImageProperties) 14B81DA1-0135-4D31-96D9-6CBFC9671A99, 34855
//
//  PropertyTagExifISOSpeed
  PKEY_Photo_ISOSpeed: TPropertyKey = (
      fmtid: (D1: $14B81DA1; D2: $0135; D3: $4D31;
      D4: ($96, $D9, $6C, $BF, $C9, $67, $1A, $99));
      pid: 34855);

//  Name:     System.Photo.LensManufacturer -- PKEY_Photo_LensManufacturer
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: E6DDCAF7-29C5-4F0A-9A68-D19412EC7090, 100
  PKEY_Photo_LensManufacturer: TPropertyKey = (
      fmtid: (D1: $E6DDCAF7; D2: $29C5; D3: $4F0A;
      D4: ($9A, $68, $D1, $94, $12, $EC, $70, $90));
      pid: 100);

//  Name:     System.Photo.LensModel -- PKEY_Photo_LensModel
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: E1277516-2B5F-4869-89B1-2E585BD38B7A, 100
  PKEY_Photo_LensModel: TPropertyKey = (
      fmtid: (D1: $E1277516; D2: $2B5F; D3: $4869;
      D4: ($89, $B1, $2E, $58, $5B, $D3, $8B, $7A));
      pid: 100);

//  Name:     System.Photo.LightSource -- PKEY_Photo_LightSource
//  Type:     UInt32 -- VT_UI4
//  FormatID: (FMTID_ImageProperties) 14B81DA1-0135-4D31-96D9-6CBFC9671A99, 37384
//
//  PropertyTagExifLightSource
  PKEY_Photo_LightSource: TPropertyKey = (
      fmtid: (D1: $14B81DA1; D2: $0135; D3: $4D31;
      D4: ($96, $D9, $6C, $BF, $C9, $67, $1A, $99));
      pid: 37384);

// Possible discrete values for PKEY_Photo_LightSource are:
  PHOTO_LIGHTSOURCE_UNKNOWN = 0;
  PHOTO_LIGHTSOURCE_DAYLIGHT = 1;
  PHOTO_LIGHTSOURCE_FLUORESCENT = 2;
  PHOTO_LIGHTSOURCE_TUNGSTEN = 3;
  PHOTO_LIGHTSOURCE_STANDARD_A = 17;
  PHOTO_LIGHTSOURCE_STANDARD_B = 18;
  PHOTO_LIGHTSOURCE_STANDARD_C = 19;
  PHOTO_LIGHTSOURCE_D55 = 20;
  PHOTO_LIGHTSOURCE_D65 = 21;
  PHOTO_LIGHTSOURCE_D75 = 22;

//  Name:     System.Photo.MakerNote -- PKEY_Photo_MakerNote
//  Type:     Buffer -- VT_VECTOR | VT_UI1  (For variants: VT_ARRAY | VT_UI1)
//  FormatID: FA303353-B659-4052-85E9-BCAC79549B84, 100
  PKEY_Photo_MakerNote: TPropertyKey = (
      fmtid: (D1: $FA303353; D2: $B659; D3: $4052;
      D4: ($85, $E9, $BC, $AC, $79, $54, $9B, $84));
      pid: 100);

//  Name:     System.Photo.MakerNoteOffset -- PKEY_Photo_MakerNoteOffset
//  Type:     UInt64 -- VT_UI8
//  FormatID: 813F4124-34E6-4D17-AB3E-6B1F3C2247A1, 100
  PKEY_Photo_MakerNoteOffset: TPropertyKey = (
      fmtid: (D1: $813F4124; D2: $34E6; D3: $4D17;
      D4: ($AB, $3E, $6B, $1F, $3C, $22, $47, $A1));
      pid: 100);

//  Name:     System.Photo.MaxAperture -- PKEY_Photo_MaxAperture
//  Type:     Double -- VT_R8
//  FormatID: 08F6D7C2-E3F2-44FC-AF1E-5AA5C81A2D3E, 100
//
//  Calculated from PKEY_Photo_MaxApertureNumerator and PKEY_Photo_MaxApertureDenominator
  PKEY_Photo_MaxAperture: TPropertyKey = (
      fmtid: (D1: $08F6D7C2; D2: $E3F2; D3: $44FC;
      D4: ($AF, $1E, $5A, $A5, $C8, $1A, $2D, $3E));
      pid: 100);

//  Name:     System.Photo.MaxApertureDenominator -- PKEY_Photo_MaxApertureDenominator
//  Type:     UInt32 -- VT_UI4
//  FormatID: C77724D4-601F-46C5-9B89-C53F93BCEB77, 100
//
//  Denominator of PKEY_Photo_MaxAperture
  PKEY_Photo_MaxApertureDenominator: TPropertyKey = (
      fmtid: (D1: $C77724D4; D2: $601F; D3: $46C5;
      D4: ($9B, $89, $C5, $3F, $93, $BC, $EB, $77));
      pid: 100);

//  Name:     System.Photo.MaxApertureNumerator -- PKEY_Photo_MaxApertureNumerator
//  Type:     UInt32 -- VT_UI4
//  FormatID: C107E191-A459-44C5-9AE6-B952AD4B906D, 100
//
//  Numerator of PKEY_Photo_MaxAperture
  PKEY_Photo_MaxApertureNumerator: TPropertyKey = (
      fmtid: (D1: $C107E191; D2: $A459; D3: $44C5;
      D4: ($9A, $E6, $B9, $52, $AD, $4B, $90, $6D));
      pid: 100);

//  Name:     System.Photo.MeteringMode -- PKEY_Photo_MeteringMode
//  Type:     UInt16 -- VT_UI2
//  FormatID: (FMTID_ImageProperties) 14B81DA1-0135-4D31-96D9-6CBFC9671A99, 37383
//
//  PropertyTagExifMeteringMode
  PKEY_Photo_MeteringMode: TPropertyKey = (
      fmtid: (D1: $14B81DA1; D2: $0135; D3: $4D31;
      D4: ($96, $D9, $6C, $BF, $C9, $67, $1A, $99));
      pid: 37383);

// Possible discrete values for PKEY_Photo_MeteringMode are:
  PHOTO_METERINGMODE_UNKNOWN = 0;
  PHOTO_METERINGMODE_AVERAGE = 1;
  PHOTO_METERINGMODE_CENTER = 2;
  PHOTO_METERINGMODE_SPOT = 3;
  PHOTO_METERINGMODE_MULTISPOT = 4;
  PHOTO_METERINGMODE_PATTERN = 5;
  PHOTO_METERINGMODE_PARTIAL = 6;

//  Name:     System.Photo.MeteringModeText -- PKEY_Photo_MeteringModeText
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: F628FD8C-7BA8-465A-A65B-C5AA79263A9E, 100
//  
//  This is the user-friendly form of System.Photo.MeteringMode.  Not intended to be parsed 
//  programmatically.
  PKEY_Photo_MeteringModeText: TPropertyKey = (
      fmtid: (D1: $F628FD8C; D2: $7BA8; D3: $465A;
      D4: ($A6, $5B, $C5, $AA, $79, $26, $3A, $9E));
      pid: 100);

//  Name:     System.Photo.Orientation -- PKEY_Photo_Orientation
//  Type:     UInt16 -- VT_UI2
//  FormatID: (FMTID_ImageProperties) 14B81DA1-0135-4D31-96D9-6CBFC9671A99, 274 (PropertyTagOrientation)
//  
//  This is the image orientation viewed in terms of rows and columns.
  PKEY_Photo_Orientation: TPropertyKey = (
      fmtid: (D1: $14B81DA1; D2: $0135; D3: $4D31;
      D4: ($96, $D9, $6C, $BF, $C9, $67, $1A, $99));
      pid: 274);

// Possible discrete values for PKEY_Photo_Orientation are:
  PHOTO_ORIENTATION_NORMAL = 1;
  PHOTO_ORIENTATION_FLIPHORIZONTAL = 2;
  PHOTO_ORIENTATION_ROTATE180 = 3;
  PHOTO_ORIENTATION_FLIPVERTICAL = 4;
  PHOTO_ORIENTATION_TRANSPOSE = 5;
  PHOTO_ORIENTATION_ROTATE270 = 6;
  PHOTO_ORIENTATION_TRANSVERSE = 7;
  PHOTO_ORIENTATION_ROTATE90 = 8;

//  Name:     System.Photo.OrientationText -- PKEY_Photo_OrientationText
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: A9EA193C-C511-498A-A06B-58E2776DCC28, 100
//  
//  This is the user-friendly form of System.Photo.Orientation.  Not intended to be parsed 
//  programmatically.
  PKEY_Photo_OrientationText: TPropertyKey = (
      fmtid: (D1: $A9EA193C; D2: $C511; D3: $498A;
      D4: ($A0, $6B, $58, $E2, $77, $6D, $CC, $28));
      pid: 100);

//  Name:     System.Photo.PhotometricInterpretation -- PKEY_Photo_PhotometricInterpretation
//  Type:     UInt16 -- VT_UI2
//  FormatID: 341796F1-1DF9-4B1C-A564-91BDEFA43877, 100
//  
//  This is the pixel composition. In JPEG compressed data, a JPEG marker is used 
//  instead of this property.
  PKEY_Photo_PhotometricInterpretation: TPropertyKey = (
      fmtid: (D1: $341796F1; D2: $1DF9; D3: $4B1C;
      D4: ($A5, $64, $91, $BD, $EF, $A4, $38, $77));
      pid: 100);

// Possible discrete values for PKEY_Photo_PhotometricInterpretation are:
  PHOTO_PHOTOMETRIC_RGB = 2;
  PHOTO_PHOTOMETRIC_YCBCR = 6;

//  Name:     System.Photo.PhotometricInterpretationText -- PKEY_Photo_PhotometricInterpretationText
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 821437D6-9EAB-4765-A589-3B1CBBD22A61, 100
//  
//  This is the user-friendly form of System.Photo.PhotometricInterpretation.  Not intended to be parsed 
//  programmatically.
  PKEY_Photo_PhotometricInterpretationText: TPropertyKey = (
      fmtid: (D1: $821437D6; D2: $9EAB; D3: $4765;
      D4: ($A5, $89, $3B, $1C, $BB, $D2, $2A, $61));
      pid: 100);

//  Name:     System.Photo.ProgramMode -- PKEY_Photo_ProgramMode
//  Type:     UInt32 -- VT_UI4
//  FormatID: 6D217F6D-3F6A-4825-B470-5F03CA2FBE9B, 100
//  
//  This is the class of the program used by the camera to set exposure when the 
//  picture is taken.
  PKEY_Photo_ProgramMode: TPropertyKey = (
      fmtid: (D1: $6D217F6D; D2: $3F6A; D3: $4825;
      D4: ($B4, $70, $5F, $03, $CA, $2F, $BE, $9B));
      pid: 100);

// Possible discrete values for PKEY_Photo_ProgramMode are:
  PHOTO_PROGRAMMODE_NOTDEFINED = 0;
  PHOTO_PROGRAMMODE_MANUAL = 1;
  PHOTO_PROGRAMMODE_NORMAL = 2;
  PHOTO_PROGRAMMODE_APERTURE = 3;
  PHOTO_PROGRAMMODE_SHUTTER = 4;
  PHOTO_PROGRAMMODE_CREATIVE = 5;
  PHOTO_PROGRAMMODE_ACTION = 6;
  PHOTO_PROGRAMMODE_PORTRAIT = 7;
  PHOTO_PROGRAMMODE_LANDSCAPE = 8;

//  Name:     System.Photo.ProgramModeText -- PKEY_Photo_ProgramModeText
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 7FE3AA27-2648-42F3-89B0-454E5CB150C3, 100
//  
//  This is the user-friendly form of System.Photo.ProgramMode.  Not intended to be parsed 
//  programmatically.
  PKEY_Photo_ProgramModeText: TPropertyKey = (
      fmtid: (D1: $7FE3AA27; D2: $2648; D3: $42F3;
      D4: ($89, $B0, $45, $4E, $5C, $B1, $50, $C3));
      pid: 100);

//  Name:     System.Photo.RelatedSoundFile -- PKEY_Photo_RelatedSoundFile
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 318A6B45-087F-4DC2-B8CC-05359551FC9E, 100
  PKEY_Photo_RelatedSoundFile: TPropertyKey = (
      fmtid: (D1: $318A6B45; D2: $087F; D3: $4DC2;
      D4: ($B8, $CC, $05, $35, $95, $51, $FC, $9E));
      pid: 100);

//  Name:     System.Photo.Saturation -- PKEY_Photo_Saturation
//  Type:     UInt32 -- VT_UI4
//  FormatID: 49237325-A95A-4F67-B211-816B2D45D2E0, 100
//  
//  This indicates the direction of saturation processing applied by the camera when 
//  the image was shot.
  PKEY_Photo_Saturation: TPropertyKey = (
      fmtid: (D1: $49237325; D2: $A95A; D3: $4F67;
      D4: ($B2, $11, $81, $6B, $2D, $45, $D2, $E0));
      pid: 100);

// Possible discrete values for PKEY_Photo_Saturation are:
  PHOTO_SATURATION_NORMAL = 0;
  PHOTO_SATURATION_LOW = 1;
  PHOTO_SATURATION_HIGH = 2;

//  Name:     System.Photo.SaturationText -- PKEY_Photo_SaturationText
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 61478C08-B600-4A84-BBE4-E99C45F0A072, 100
//  
//  This is the user-friendly form of System.Photo.Saturation.  Not intended to be parsed 
//  programmatically.
  PKEY_Photo_SaturationText: TPropertyKey = (
      fmtid: (D1: $61478C08; D2: $B600; D3: $4A84;
      D4: ($BB, $E4, $E9, $9C, $45, $F0, $A0, $72));
      pid: 100);

//  Name:     System.Photo.Sharpness -- PKEY_Photo_Sharpness
//  Type:     UInt32 -- VT_UI4
//  FormatID: FC6976DB-8349-4970-AE97-B3C5316A08F0, 100
//  
//  This indicates the direction of sharpness processing applied by the camera when 
//  the image was shot.
  PKEY_Photo_Sharpness: TPropertyKey = (
      fmtid: (D1: $FC6976DB; D2: $8349; D3: $4970;
      D4: ($AE, $97, $B3, $C5, $31, $6A, $08, $F0));
      pid: 100);

// Possible discrete values for PKEY_Photo_Sharpness are:
  PHOTO_SHARPNESS_NORMAL = 0;
  PHOTO_SHARPNESS_SOFT = 1;
  PHOTO_SHARPNESS_HARD = 2;

//  Name:     System.Photo.SharpnessText -- PKEY_Photo_SharpnessText
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 51EC3F47-DD50-421D-8769-334F50424B1E, 100
//  
//  This is the user-friendly form of System.Photo.Sharpness.  Not intended to be parsed 
//  programmatically.
  PKEY_Photo_SharpnessText: TPropertyKey = (
      fmtid: (D1: $51EC3F47; D2: $DD50; D3: $421D;
      D4: ($87, $69, $33, $4F, $50, $42, $4B, $1E));
      pid: 100);

//  Name:     System.Photo.ShutterSpeed -- PKEY_Photo_ShutterSpeed
//  Type:     Double -- VT_R8
//  FormatID: (FMTID_ImageProperties) 14B81DA1-0135-4D31-96D9-6CBFC9671A99, 37377
//
//  PropertyTagExifShutterSpeed.  Calculated from PKEY_Photo_ShutterSpeedNumerator and PKEY_Photo_ShutterSpeedDenominator
  PKEY_Photo_ShutterSpeed: TPropertyKey = (
      fmtid: (D1: $14B81DA1; D2: $0135; D3: $4D31;
      D4: ($96, $D9, $6C, $BF, $C9, $67, $1A, $99));
      pid: 37377);

//  Name:     System.Photo.ShutterSpeedDenominator -- PKEY_Photo_ShutterSpeedDenominator
//  Type:     Int32 -- VT_I4
//  FormatID: E13D8975-81C7-4948-AE3F-37CAE11E8FF7, 100
//
//  Denominator of PKEY_Photo_ShutterSpeed
  PKEY_Photo_ShutterSpeedDenominator: TPropertyKey = (
      fmtid: (D1: $E13D8975; D2: $81C7; D3: $4948;
      D4: ($AE, $3F, $37, $CA, $E1, $1E, $8F, $F7));
      pid: 100);

//  Name:     System.Photo.ShutterSpeedNumerator -- PKEY_Photo_ShutterSpeedNumerator
//  Type:     Int32 -- VT_I4
//  FormatID: 16EA4042-D6F4-4BCA-8349-7C78D30FB333, 100
//
//  Numerator of PKEY_Photo_ShutterSpeed
  PKEY_Photo_ShutterSpeedNumerator: TPropertyKey = (
      fmtid: (D1: $16EA4042; D2: $D6F4; D3: $4BCA;
      D4: ($83, $49, $7C, $78, $D3, $0F, $B3, $33));
      pid: 100);

//  Name:     System.Photo.SubjectDistance -- PKEY_Photo_SubjectDistance
//  Type:     Double -- VT_R8
//  FormatID: (FMTID_ImageProperties) 14B81DA1-0135-4D31-96D9-6CBFC9671A99, 37382
//
//  PropertyTagExifSubjectDist.  Calculated from PKEY_Photo_SubjectDistanceNumerator and PKEY_Photo_SubjectDistanceDenominator
  PKEY_Photo_SubjectDistance: TPropertyKey = (
      fmtid: (D1: $14B81DA1; D2: $0135; D3: $4D31;
      D4: ($96, $D9, $6C, $BF, $C9, $67, $1A, $99));
      pid: 37382);

//  Name:     System.Photo.SubjectDistanceDenominator -- PKEY_Photo_SubjectDistanceDenominator
//  Type:     UInt32 -- VT_UI4
//  FormatID: 0C840A88-B043-466D-9766-D4B26DA3FA77, 100
//
//  Denominator of PKEY_Photo_SubjectDistance
  PKEY_Photo_SubjectDistanceDenominator: TPropertyKey = (
      fmtid: (D1: $0C840A88; D2: $B043; D3: $466D;
      D4: ($97, $66, $D4, $B2, $6D, $A3, $FA, $77));
      pid: 100);

//  Name:     System.Photo.SubjectDistanceNumerator -- PKEY_Photo_SubjectDistanceNumerator
//  Type:     UInt32 -- VT_UI4
//  FormatID: 8AF4961C-F526-43E5-AA81-DB768219178D, 100
//
//  Numerator of PKEY_Photo_SubjectDistance
  PKEY_Photo_SubjectDistanceNumerator: TPropertyKey = (
      fmtid: (D1: $8AF4961C; D2: $F526; D3: $43E5;
      D4: ($AA, $81, $DB, $76, $82, $19, $17, $8D));
      pid: 100);

//  Name:     System.Photo.TranscodedForSync -- PKEY_Photo_TranscodedForSync
//  Type:     Boolean -- VT_BOOL
//  FormatID: 9A8EBB75-6458-4E82-BACB-35C0095B03BB, 100
  PKEY_Photo_TranscodedForSync: TPropertyKey = (
      fmtid: (D1: $9A8EBB75; D2: $6458; D3: $4E82;
      D4: ($BA, $CB, $35, $C0, $09, $5B, $03, $BB));
      pid: 100);

//  Name:     System.Photo.WhiteBalance -- PKEY_Photo_WhiteBalance
//  Type:     UInt32 -- VT_UI4
//  FormatID: EE3D3D8A-5381-4CFA-B13B-AAF66B5F4EC9, 100
//  
//  This indicates the white balance mode set when the image was shot.
  PKEY_Photo_WhiteBalance: TPropertyKey = (
      fmtid: (D1: $EE3D3D8A; D2: $5381; D3: $4CFA;
      D4: ($B1, $3B, $AA, $F6, $6B, $5F, $4E, $C9));
      pid: 100);

// Possible discrete values for PKEY_Photo_WhiteBalance are:
  PHOTO_WHITEBALANCE_AUTO = 0;
  PHOTO_WHITEBALANCE_MANUAL = 1;

//  Name:     System.Photo.WhiteBalanceText -- PKEY_Photo_WhiteBalanceText
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 6336B95E-C7A7-426D-86FD-7AE3D39C84B4, 100
//  
//  This is the user-friendly form of System.Photo.WhiteBalance.  Not intended to be parsed 
//  programmatically.
  PKEY_Photo_WhiteBalanceText: TPropertyKey = (
      fmtid: (D1: $6336B95E; D2: $C7A7; D3: $426D;
      D4: ($86, $FD, $7A, $E3, $D3, $9C, $84, $B4));
      pid: 100);
 
//-----------------------------------------------------------------------------
// PropGroup properties

//  Name:     System.PropGroup.Advanced -- PKEY_PropGroup_Advanced
//  Type:     Null -- VT_NULL
//  FormatID: 900A403B-097B-4B95-8AE2-071FDAEEB118, 100
  PKEY_PropGroup_Advanced: TPropertyKey = (
      fmtid: (D1: $900A403B; D2: $097B; D3: $4B95;
      D4: ($8A, $E2, $07, $1F, $DA, $EE, $B1, $18));
      pid: 100);

//  Name:     System.PropGroup.Audio -- PKEY_PropGroup_Audio
//  Type:     Null -- VT_NULL
//  FormatID: 2804D469-788F-48AA-8570-71B9C187E138, 100
  PKEY_PropGroup_Audio: TPropertyKey = (
      fmtid: (D1: $2804D469; D2: $788F; D3: $48AA;
      D4: ($85, $70, $71, $B9, $C1, $87, $E1, $38));
      pid: 100);

//  Name:     System.PropGroup.Calendar -- PKEY_PropGroup_Calendar
//  Type:     Null -- VT_NULL
//  FormatID: 9973D2B5-BFD8-438A-BA94-5349B293181A, 100
  PKEY_PropGroup_Calendar: TPropertyKey = (
      fmtid: (D1: $9973D2B5; D2: $BFD8; D3: $438A;
      D4: ($BA, $94, $53, $49, $B2, $93, $18, $1A));
      pid: 100);

//  Name:     System.PropGroup.Camera -- PKEY_PropGroup_Camera
//  Type:     Null -- VT_NULL
//  FormatID: DE00DE32-547E-4981-AD4B-542F2E9007D8, 100
  PKEY_PropGroup_Camera: TPropertyKey = (
      fmtid: (D1: $DE00DE32; D2: $547E; D3: $4981;
      D4: ($AD, $4B, $54, $2F, $2E, $90, $07, $D8));
      pid: 100);

//  Name:     System.PropGroup.Contact -- PKEY_PropGroup_Contact
//  Type:     Null -- VT_NULL
//  FormatID: DF975FD3-250A-4004-858F-34E29A3E37AA, 100
  PKEY_PropGroup_Contact: TPropertyKey = (
      fmtid: (D1: $DF975FD3; D2: $250A; D3: $4004;
      D4: ($85, $8F, $34, $E2, $9A, $3E, $37, $AA));
      pid: 100);

//  Name:     System.PropGroup.Content -- PKEY_PropGroup_Content
//  Type:     Null -- VT_NULL
//  FormatID: D0DAB0BA-368A-4050-A882-6C010FD19A4F, 100
  PKEY_PropGroup_Content: TPropertyKey = (
      fmtid: (D1: $D0DAB0BA; D2: $368A; D3: $4050;
      D4: ($A8, $82, $6C, $01, $0F, $D1, $9A, $4F));
      pid: 100);

//  Name:     System.PropGroup.Description -- PKEY_PropGroup_Description
//  Type:     Null -- VT_NULL
//  FormatID: 8969B275-9475-4E00-A887-FF93B8B41E44, 100
  PKEY_PropGroup_Description: TPropertyKey = (
      fmtid: (D1: $8969B275; D2: $9475; D3: $4E00;
      D4: ($A8, $87, $FF, $93, $B8, $B4, $1E, $44));
      pid: 100);

//  Name:     System.PropGroup.FileSystem -- PKEY_PropGroup_FileSystem
//  Type:     Null -- VT_NULL
//  FormatID: E3A7D2C1-80FC-4B40-8F34-30EA111BDC2E, 100
  PKEY_PropGroup_FileSystem: TPropertyKey = (
      fmtid: (D1: $E3A7D2C1; D2: $80FC; D3: $4B40;
      D4: ($8F, $34, $30, $EA, $11, $1B, $DC, $2E));
      pid: 100);

//  Name:     System.PropGroup.General -- PKEY_PropGroup_General
//  Type:     Null -- VT_NULL
//  FormatID: CC301630-B192-4C22-B372-9F4C6D338E07, 100
  PKEY_PropGroup_General: TPropertyKey = (
      fmtid: (D1: $CC301630; D2: $B192; D3: $4C22;
      D4: ($B3, $72, $9F, $4C, $6D, $33, $8E, $07));
      pid: 100);

//  Name:     System.PropGroup.GPS -- PKEY_PropGroup_GPS
//  Type:     Null -- VT_NULL
//  FormatID: F3713ADA-90E3-4E11-AAE5-FDC17685B9BE, 100
  PKEY_PropGroup_GPS: TPropertyKey = (
      fmtid: (D1: $F3713ADA; D2: $90E3; D3: $4E11;
      D4: ($AA, $E5, $FD, $C1, $76, $85, $B9, $BE));
      pid: 100);

//  Name:     System.PropGroup.Image -- PKEY_PropGroup_Image
//  Type:     Null -- VT_NULL
//  FormatID: E3690A87-0FA8-4A2A-9A9F-FCE8827055AC, 100
  PKEY_PropGroup_Image: TPropertyKey = (
      fmtid: (D1: $E3690A87; D2: $0FA8; D3: $4A2A;
      D4: ($9A, $9F, $FC, $E8, $82, $70, $55, $AC));
      pid: 100);

//  Name:     System.PropGroup.Media -- PKEY_PropGroup_Media
//  Type:     Null -- VT_NULL
//  FormatID: 61872CF7-6B5E-4B4B-AC2D-59DA84459248, 100
  PKEY_PropGroup_Media: TPropertyKey = (
      fmtid: (D1: $61872CF7; D2: $6B5E; D3: $4B4B;
      D4: ($AC, $2D, $59, $DA, $84, $45, $92, $48));
      pid: 100);

//  Name:     System.PropGroup.MediaAdvanced -- PKEY_PropGroup_MediaAdvanced
//  Type:     Null -- VT_NULL
//  FormatID: 8859A284-DE7E-4642-99BA-D431D044B1EC, 100
  PKEY_PropGroup_MediaAdvanced: TPropertyKey = (
      fmtid: (D1: $8859A284; D2: $DE7E; D3: $4642;
      D4: ($99, $BA, $D4, $31, $D0, $44, $B1, $EC));
      pid: 100);

//  Name:     System.PropGroup.Message -- PKEY_PropGroup_Message
//  Type:     Null -- VT_NULL
//  FormatID: 7FD7259D-16B4-4135-9F97-7C96ECD2FA9E, 100
  PKEY_PropGroup_Message: TPropertyKey = (
      fmtid: (D1: $7FD7259D; D2: $16B4; D3: $4135;
      D4: ($9F, $97, $7C, $96, $EC, $D2, $FA, $9E));
      pid: 100);

//  Name:     System.PropGroup.Music -- PKEY_PropGroup_Music
//  Type:     Null -- VT_NULL
//  FormatID: 68DD6094-7216-40F1-A029-43FE7127043F, 100
  PKEY_PropGroup_Music: TPropertyKey = (
      fmtid: (D1: $68DD6094; D2: $7216; D3: $40F1;
      D4: ($A0, $29, $43, $FE, $71, $27, $04, $3F));
      pid: 100);

//  Name:     System.PropGroup.Origin -- PKEY_PropGroup_Origin
//  Type:     Null -- VT_NULL
//  FormatID: 2598D2FB-5569-4367-95DF-5CD3A177E1A5, 100
  PKEY_PropGroup_Origin: TPropertyKey = (
      fmtid: (D1: $2598D2FB; D2: $5569; D3: $4367;
      D4: ($95, $DF, $5C, $D3, $A1, $77, $E1, $A5));
      pid: 100);

//  Name:     System.PropGroup.PhotoAdvanced -- PKEY_PropGroup_PhotoAdvanced
//  Type:     Null -- VT_NULL
//  FormatID: 0CB2BF5A-9EE7-4A86-8222-F01E07FDADAF, 100
  PKEY_PropGroup_PhotoAdvanced: TPropertyKey = (
      fmtid: (D1: $0CB2BF5A; D2: $9EE7; D3: $4A86;
      D4: ($82, $22, $F0, $1E, $07, $FD, $AD, $AF));
      pid: 100);

//  Name:     System.PropGroup.RecordedTV -- PKEY_PropGroup_RecordedTV
//  Type:     Null -- VT_NULL
//  FormatID: E7B33238-6584-4170-A5C0-AC25EFD9DA56, 100
  PKEY_PropGroup_RecordedTV: TPropertyKey = (
      fmtid: (D1: $E7B33238; D2: $6584; D3: $4170;
      D4: ($A5, $C0, $AC, $25, $EF, $D9, $DA, $56));
      pid: 100);

//  Name:     System.PropGroup.Video -- PKEY_PropGroup_Video
//  Type:     Null -- VT_NULL
//  FormatID: BEBE0920-7671-4C54-A3EB-49FDDFC191EE, 100
  PKEY_PropGroup_Video: TPropertyKey = (
      fmtid: (D1: $BEBE0920; D2: $7671; D3: $4C54;
      D4: ($A3, $EB, $49, $FD, $DF, $C1, $91, $EE));
      pid: 100);
 
//-----------------------------------------------------------------------------
// PropList properties



//  Name:     System.PropList.ConflictPrompt -- PKEY_PropList_ConflictPrompt
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: C9944A21-A406-48FE-8225-AEC7E24C211B, 11
//  
//  The list of properties to show in the file operation conflict resolution dialog. Properties with empty 
//  values will not be displayed. Register under the regvalue of "ConflictPrompt".
  PKEY_PropList_ConflictPrompt: TPropertyKey = (
      fmtid: (D1: $C9944A21; D2: $A406; D3: $48FE;
      D4: ($82, $25, $AE, $C7, $E2, $4C, $21, $1B));
      pid: 11);

//  Name:     System.PropList.ExtendedTileInfo -- PKEY_PropList_ExtendedTileInfo
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: C9944A21-A406-48FE-8225-AEC7E24C211B, 9
//  
//  The list of properties to show in the listview on extended tiles. Register under the regvalue of 
//  "ExtendedTileInfo".
  PKEY_PropList_ExtendedTileInfo: TPropertyKey = (
      fmtid: (D1: $C9944A21; D2: $A406; D3: $48FE;
      D4: ($82, $25, $AE, $C7, $E2, $4C, $21, $1B));
      pid: 9);

//  Name:     System.PropList.FileOperationPrompt -- PKEY_PropList_FileOperationPrompt
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: C9944A21-A406-48FE-8225-AEC7E24C211B, 10
//  
//  The list of properties to show in the file operation confirmation dialog. Properties with empty values 
//  will not be displayed. If this list is not specified, then the InfoTip property list is used instead. 
//  Register under the regvalue of "FileOperationPrompt".
  PKEY_PropList_FileOperationPrompt: TPropertyKey = (
      fmtid: (D1: $C9944A21; D2: $A406; D3: $48FE;
      D4: ($82, $25, $AE, $C7, $E2, $4C, $21, $1B));
      pid: 10);

//  Name:     System.PropList.FullDetails -- PKEY_PropList_FullDetails
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: C9944A21-A406-48FE-8225-AEC7E24C211B, 2
//  
//  The list of all the properties to show in the details page.  Property groups can be included in this list 
//  in order to more easily organize the UI.  Register under the regvalue of "FullDetails".
  PKEY_PropList_FullDetails: TPropertyKey = (
      fmtid: (D1: $C9944A21; D2: $A406; D3: $48FE;
      D4: ($82, $25, $AE, $C7, $E2, $4C, $21, $1B));
      pid: 2);

//  Name:     System.PropList.InfoTip -- PKEY_PropList_InfoTip
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: C9944A21-A406-48FE-8225-AEC7E24C211B, 4 (PID_PROPLIST_INFOTIP)
//  
//  The list of properties to show in the infotip. Properties with empty values will not be displayed. Register 
//  under the regvalue of "InfoTip".
  PKEY_PropList_InfoTip: TPropertyKey = (
      fmtid: (D1: $C9944A21; D2: $A406; D3: $48FE;
      D4: ($82, $25, $AE, $C7, $E2, $4C, $21, $1B));
      pid: 4);

//  Name:     System.PropList.NonPersonal -- PKEY_PropList_NonPersonal
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 49D1091F-082E-493F-B23F-D2308AA9668C, 100
//  
//  The list of properties that are considered 'non-personal'. When told to remove all non-personal properties 
//  from a given file, the system will leave these particular properties untouched. Register under the regvalue 
//  of "NonPersonal".
  PKEY_PropList_NonPersonal: TPropertyKey = (
      fmtid: (D1: $49D1091F; D2: $082E; D3: $493F;
      D4: ($B2, $3F, $D2, $30, $8A, $A9, $66, $8C));
      pid: 100);

//  Name:     System.PropList.PreviewDetails -- PKEY_PropList_PreviewDetails
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: C9944A21-A406-48FE-8225-AEC7E24C211B, 8
//
//  The list of properties to display in the preview pane.  Register under the regvalue of "PreviewDetails".
  PKEY_PropList_PreviewDetails: TPropertyKey = (
      fmtid: (D1: $C9944A21; D2: $A406; D3: $48FE;
      D4: ($82, $25, $AE, $C7, $E2, $4C, $21, $1B));
      pid: 8);

//  Name:     System.PropList.PreviewTitle -- PKEY_PropList_PreviewTitle
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: C9944A21-A406-48FE-8225-AEC7E24C211B, 6
//  
//  The one or two properties to display in the preview pane title section.  The optional second property is 
//  displayed as a subtitle.  Register under the regvalue of "PreviewTitle".
  PKEY_PropList_PreviewTitle: TPropertyKey = (
      fmtid: (D1: $C9944A21; D2: $A406; D3: $48FE;
      D4: ($82, $25, $AE, $C7, $E2, $4C, $21, $1B));
      pid: 6);

//  Name:     System.PropList.QuickTip -- PKEY_PropList_QuickTip
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: C9944A21-A406-48FE-8225-AEC7E24C211B, 5 (PID_PROPLIST_QUICKTIP)
//  
//  The list of properties to show in the infotip when the item is on a slow network. Properties with empty 
//  values will not be displayed. Register under the regvalue of "QuickTip".
  PKEY_PropList_QuickTip: TPropertyKey = (
      fmtid: (D1: $C9944A21; D2: $A406; D3: $48FE;
      D4: ($82, $25, $AE, $C7, $E2, $4C, $21, $1B));
      pid: 5);

//  Name:     System.PropList.TileInfo -- PKEY_PropList_TileInfo
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: C9944A21-A406-48FE-8225-AEC7E24C211B, 3 (PID_PROPLIST_TILEINFO)
//  
//  The list of properties to show in the listview on tiles. Register under the regvalue of "TileInfo".
  PKEY_PropList_TileInfo: TPropertyKey = (
      fmtid: (D1: $C9944A21; D2: $A406; D3: $48FE;
      D4: ($82, $25, $AE, $C7, $E2, $4C, $21, $1B));
      pid: 3);

//  Name:     System.PropList.XPDetailsPanel -- PKEY_PropList_XPDetailsPanel
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_WebView) F2275480-F782-4291-BD94-F13693513AEC, 0 (PID_DISPLAY_PROPERTIES)
//
//  The list of properties to display in the XP webview details panel. Obsolete.
  PKEY_PropList_XPDetailsPanel: TPropertyKey = (
      fmtid: (D1: $F2275480; D2: $F782; D3: $4291;
      D4: ($BD, $94, $F1, $36, $93, $51, $3A, $EC));
      pid: 0);
 
//-----------------------------------------------------------------------------
// RecordedTV properties



//  Name:     System.RecordedTV.ChannelNumber -- PKEY_RecordedTV_ChannelNumber
//  Type:     UInt32 -- VT_UI4
//  FormatID: 6D748DE2-8D38-4CC3-AC60-F009B057C557, 7
//
//  Example: 42
  PKEY_RecordedTV_ChannelNumber: TPropertyKey = (
      fmtid: (D1: $6D748DE2; D2: $8D38; D3: $4CC3;
      D4: ($AC, $60, $F0, $09, $B0, $57, $C5, $57));
      pid: 7);

//  Name:     System.RecordedTV.Credits -- PKEY_RecordedTV_Credits
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 6D748DE2-8D38-4CC3-AC60-F009B057C557, 4
//
//  Example: "Don Messick/Frank Welker/Casey Kasem/Heather North/Nicole Jaffe;;;"
  PKEY_RecordedTV_Credits: TPropertyKey = (
      fmtid: (D1: $6D748DE2; D2: $8D38; D3: $4CC3;
      D4: ($AC, $60, $F0, $09, $B0, $57, $C5, $57));
      pid: 4);

//  Name:     System.RecordedTV.DateContentExpires -- PKEY_RecordedTV_DateContentExpires
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: 6D748DE2-8D38-4CC3-AC60-F009B057C557, 15
  PKEY_RecordedTV_DateContentExpires: TPropertyKey = (
      fmtid: (D1: $6D748DE2; D2: $8D38; D3: $4CC3;
      D4: ($AC, $60, $F0, $09, $B0, $57, $C5, $57));
      pid: 15);

//  Name:     System.RecordedTV.EpisodeName -- PKEY_RecordedTV_EpisodeName
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 6D748DE2-8D38-4CC3-AC60-F009B057C557, 2
//
//  Example: "Nowhere to Hyde"
  PKEY_RecordedTV_EpisodeName: TPropertyKey = (
      fmtid: (D1: $6D748DE2; D2: $8D38; D3: $4CC3;
      D4: ($AC, $60, $F0, $09, $B0, $57, $C5, $57));
      pid: 2);

//  Name:     System.RecordedTV.IsATSCContent -- PKEY_RecordedTV_IsATSCContent
//  Type:     Boolean -- VT_BOOL
//  FormatID: 6D748DE2-8D38-4CC3-AC60-F009B057C557, 16
  PKEY_RecordedTV_IsATSCContent: TPropertyKey = (
      fmtid: (D1: $6D748DE2; D2: $8D38; D3: $4CC3;
      D4: ($AC, $60, $F0, $09, $B0, $57, $C5, $57));
      pid: 16);

//  Name:     System.RecordedTV.IsClosedCaptioningAvailable -- PKEY_RecordedTV_IsClosedCaptioningAvailable
//  Type:     Boolean -- VT_BOOL
//  FormatID: 6D748DE2-8D38-4CC3-AC60-F009B057C557, 12
  PKEY_RecordedTV_IsClosedCaptioningAvailable: TPropertyKey = (
      fmtid: (D1: $6D748DE2; D2: $8D38; D3: $4CC3;
      D4: ($AC, $60, $F0, $09, $B0, $57, $C5, $57));
      pid: 12);

//  Name:     System.RecordedTV.IsDTVContent -- PKEY_RecordedTV_IsDTVContent
//  Type:     Boolean -- VT_BOOL
//  FormatID: 6D748DE2-8D38-4CC3-AC60-F009B057C557, 17
  PKEY_RecordedTV_IsDTVContent: TPropertyKey = (
      fmtid: (D1: $6D748DE2; D2: $8D38; D3: $4CC3;
      D4: ($AC, $60, $F0, $09, $B0, $57, $C5, $57));
      pid: 17);

//  Name:     System.RecordedTV.IsHDContent -- PKEY_RecordedTV_IsHDContent
//  Type:     Boolean -- VT_BOOL
//  FormatID: 6D748DE2-8D38-4CC3-AC60-F009B057C557, 18
  PKEY_RecordedTV_IsHDContent: TPropertyKey = (
      fmtid: (D1: $6D748DE2; D2: $8D38; D3: $4CC3;
      D4: ($AC, $60, $F0, $09, $B0, $57, $C5, $57));
      pid: 18);

//  Name:     System.RecordedTV.IsRepeatBroadcast -- PKEY_RecordedTV_IsRepeatBroadcast
//  Type:     Boolean -- VT_BOOL
//  FormatID: 6D748DE2-8D38-4CC3-AC60-F009B057C557, 13
  PKEY_RecordedTV_IsRepeatBroadcast: TPropertyKey = (
      fmtid: (D1: $6D748DE2; D2: $8D38; D3: $4CC3;
      D4: ($AC, $60, $F0, $09, $B0, $57, $C5, $57));
      pid: 13);

//  Name:     System.RecordedTV.IsSAP -- PKEY_RecordedTV_IsSAP
//  Type:     Boolean -- VT_BOOL
//  FormatID: 6D748DE2-8D38-4CC3-AC60-F009B057C557, 14
  PKEY_RecordedTV_IsSAP: TPropertyKey = (
      fmtid: (D1: $6D748DE2; D2: $8D38; D3: $4CC3;
      D4: ($AC, $60, $F0, $09, $B0, $57, $C5, $57));
      pid: 14);

//  Name:     System.RecordedTV.NetworkAffiliation -- PKEY_RecordedTV_NetworkAffiliation
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 2C53C813-FB63-4E22-A1AB-0B331CA1E273, 100
  PKEY_RecordedTV_NetworkAffiliation: TPropertyKey = (
      fmtid: (D1: $2C53C813; D2: $FB63; D3: $4E22;
      D4: ($A1, $AB, $0B, $33, $1C, $A1, $E2, $73));
      pid: 100);

//  Name:     System.RecordedTV.OriginalBroadcastDate -- PKEY_RecordedTV_OriginalBroadcastDate
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: 4684FE97-8765-4842-9C13-F006447B178C, 100
  PKEY_RecordedTV_OriginalBroadcastDate: TPropertyKey = (
      fmtid: (D1: $4684FE97; D2: $8765; D3: $4842;
      D4: ($9C, $13, $F0, $06, $44, $7B, $17, $8C));
      pid: 100);

//  Name:     System.RecordedTV.ProgramDescription -- PKEY_RecordedTV_ProgramDescription
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 6D748DE2-8D38-4CC3-AC60-F009B057C557, 3
  PKEY_RecordedTV_ProgramDescription: TPropertyKey = (
      fmtid: (D1: $6D748DE2; D2: $8D38; D3: $4CC3;
      D4: ($AC, $60, $F0, $09, $B0, $57, $C5, $57));
      pid: 3);

//  Name:     System.RecordedTV.RecordingTime -- PKEY_RecordedTV_RecordingTime
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: A5477F61-7A82-4ECA-9DDE-98B69B2479B3, 100
  PKEY_RecordedTV_RecordingTime: TPropertyKey = (
      fmtid: (D1: $A5477F61; D2: $7A82; D3: $4ECA;
      D4: ($9D, $DE, $98, $B6, $9B, $24, $79, $B3));
      pid: 100);

//  Name:     System.RecordedTV.StationCallSign -- PKEY_RecordedTV_StationCallSign
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 6D748DE2-8D38-4CC3-AC60-F009B057C557, 5
//
//  Example: "TOONP"
  PKEY_RecordedTV_StationCallSign: TPropertyKey = (
      fmtid: (D1: $6D748DE2; D2: $8D38; D3: $4CC3;
      D4: ($AC, $60, $F0, $09, $B0, $57, $C5, $57));
      pid: 5);

//  Name:     System.RecordedTV.StationName -- PKEY_RecordedTV_StationName
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 1B5439E7-EBA1-4AF8-BDD7-7AF1D4549493, 100
  PKEY_RecordedTV_StationName: TPropertyKey = (
      fmtid: (D1: $1B5439E7; D2: $EBA1; D3: $4AF8;
      D4: ($BD, $D7, $7A, $F1, $D4, $54, $94, $93));
      pid: 100);
 
//-----------------------------------------------------------------------------
// Search properties



//  Name:     System.Search.AutoSummary -- PKEY_Search_AutoSummary
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 560C36C0-503A-11CF-BAA1-00004C752A9A, 2
//
//  General Summary of the document.
  PKEY_Search_AutoSummary: TPropertyKey = (
      fmtid: (D1: $560C36C0; D2: $503A; D3: $11CF;
      D4: ($BA, $A1, $00, $00, $4C, $75, $2A, $9A));
      pid: 2);

//  Name:     System.Search.ContainerHash -- PKEY_Search_ContainerHash
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: BCEEE283-35DF-4D53-826A-F36A3EEFC6BE, 100
//
//  Hash code used to identify attachments to be deleted based on a common container url
  PKEY_Search_ContainerHash: TPropertyKey = (
      fmtid: (D1: $BCEEE283; D2: $35DF; D3: $4D53;
      D4: ($82, $6A, $F3, $6A, $3E, $EF, $C6, $BE));
      pid: 100);

//  Name:     System.Search.Contents -- PKEY_Search_Contents
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_Storage) B725F130-47EF-101A-A5F1-02608C9EEBAC, 19 (PID_STG_CONTENTS)
//  
//  The contents of the item. This property is for query restrictions only; it cannot be retrieved in a 
//  query result. The Indexing Service friendly name is 'contents'.
  PKEY_Search_Contents: TPropertyKey = (
      fmtid: (D1: $B725F130; D2: $47EF; D3: $101A;
      D4: ($A5, $F1, $02, $60, $8C, $9E, $EB, $AC));
      pid: 19);

//  Name:     System.Search.EntryID -- PKEY_Search_EntryID
//  Type:     Int32 -- VT_I4
//  FormatID: (FMTID_Query) 49691C90-7E17-101A-A91C-08002B2ECDA9, 5 (PROPID_QUERY_WORKID)
//  
//  The entry ID for an item within a given catalog in the Windows Search Index.
//  This value may be recycled, and therefore is not considered unique over time.
  PKEY_Search_EntryID: TPropertyKey = (
      fmtid: (D1: $49691C90; D2: $7E17; D3: $101A;
      D4: ($A9, $1C, $08, $00, $2B, $2E, $CD, $A9));
      pid: 5);

//  Name:     System.Search.GatherTime -- PKEY_Search_GatherTime
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: 0B63E350-9CCC-11D0-BCDB-00805FCCCE04, 8
//
//  The Datetime that the Windows Search Gatherer process last pushed properties of this document to the Windows Search Gatherer Plugins.
  PKEY_Search_GatherTime: TPropertyKey = (
      fmtid: (D1: $0B63E350; D2: $9CCC; D3: $11D0;
      D4: ($BC, $DB, $00, $80, $5F, $CC, $CE, $04));
      pid: 8);

//  Name:     System.Search.IsClosedDirectory -- PKEY_Search_IsClosedDirectory
//  Type:     Boolean -- VT_BOOL
//  FormatID: 0B63E343-9CCC-11D0-BCDB-00805FCCCE04, 23
//
//  If this property is emitted with a value of TRUE, then it indicates that this URL's last modified time applies to all of it's children, and if this URL is deleted then all of it's children are deleted as well.  For example, this would be emitted as TRUE when emitting the URL of an email so that all attachments are tied to the last modified time of that email.
  PKEY_Search_IsClosedDirectory: TPropertyKey = (
      fmtid: (D1: $0B63E343; D2: $9CCC; D3: $11D0;
      D4: ($BC, $DB, $00, $80, $5F, $CC, $CE, $04));
      pid: 23);

//  Name:     System.Search.IsFullyContained -- PKEY_Search_IsFullyContained
//  Type:     Boolean -- VT_BOOL
//  FormatID: 0B63E343-9CCC-11D0-BCDB-00805FCCCE04, 24
//
//  Any child URL of a URL which has System.Search.IsClosedDirectory=TRUE must emit System.Search.IsFullyContained=TRUE.  This ensures that the URL is not deleted at the end of a crawl because it hasn't been visited (which is the normal mechanism for detecting deletes).  For example an email attachment would emit this property
  PKEY_Search_IsFullyContained: TPropertyKey = (
      fmtid: (D1: $0B63E343; D2: $9CCC; D3: $11D0;
      D4: ($BC, $DB, $00, $80, $5F, $CC, $CE, $04));
      pid: 24);

//  Name:     System.Search.QueryFocusedSummary -- PKEY_Search_QueryFocusedSummary
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 560C36C0-503A-11CF-BAA1-00004C752A9A, 3
//
//  Query Focused Summary of the document.
  PKEY_Search_QueryFocusedSummary: TPropertyKey = (
      fmtid: (D1: $560C36C0; D2: $503A; D3: $11CF;
      D4: ($BA, $A1, $00, $00, $4C, $75, $2A, $9A));
      pid: 3);

//  Name:     System.Search.Rank -- PKEY_Search_Rank
//  Type:     Int32 -- VT_I4
//  FormatID: (FMTID_Query) 49691C90-7E17-101A-A91C-08002B2ECDA9, 3 (PROPID_QUERY_RANK)
//  
//  Relevance rank of row. Ranges from 0-1000. Larger numbers = better matches.  Query-time only, not 
//  defined in Search schema, retrievable but not searchable.
  PKEY_Search_Rank: TPropertyKey = (
      fmtid: (D1: $49691C90; D2: $7E17; D3: $101A;
      D4: ($A9, $1C, $08, $00, $2B, $2E, $CD, $A9));
      pid: 3);

//  Name:     System.Search.Store -- PKEY_Search_Store
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: A06992B3-8CAF-4ED7-A547-B259E32AC9FC, 100
//
//  The identifier for the protocol handler that produced this item. (E.g. MAPI, CSC, FILE etc.)
  PKEY_Search_Store: TPropertyKey = (
      fmtid: (D1: $A06992B3; D2: $8CAF; D3: $4ED7;
      D4: ($A5, $47, $B2, $59, $E3, $2A, $C9, $FC));
      pid: 100);

//  Name:     System.Search.UrlToIndex -- PKEY_Search_UrlToIndex
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 0B63E343-9CCC-11D0-BCDB-00805FCCCE04, 2
//
//  This property should be emitted by a container IFilter for each child URL within the container.  The children will eventually be crawled by the indexer if they are within scope.
  PKEY_Search_UrlToIndex: TPropertyKey = (
      fmtid: (D1: $0B63E343; D2: $9CCC; D3: $11D0;
      D4: ($BC, $DB, $00, $80, $5F, $CC, $CE, $04));
      pid: 2);

//  Name:     System.Search.UrlToIndexWithModificationTime -- PKEY_Search_UrlToIndexWithModificationTime
//  Type:     Multivalue Any -- VT_VECTOR | VT_NULL  (For variants: VT_ARRAY | VT_NULL)
//  FormatID: 0B63E343-9CCC-11D0-BCDB-00805FCCCE04, 12
//
//  This property is the same as System.Search.UrlToIndex except that it includes the time the URL was last modified.  This is an optimization for the indexer as it doesn't have to call back into the protocol handler to ask for this information to determine if the content needs to be indexed again.  The property is a vector with two elements, a VT_LPWSTR with the URL and a VT_FILETIME for the last modified time.
  PKEY_Search_UrlToIndexWithModificationTime: TPropertyKey = (
      fmtid: (D1: $0B63E343; D2: $9CCC; D3: $11D0;
      D4: ($BC, $DB, $00, $80, $5F, $CC, $CE, $04));
      pid: 12);
 
//-----------------------------------------------------------------------------
// Shell properties



//  Name:     System.DescriptionID -- PKEY_DescriptionID
//  Type:     Buffer -- VT_VECTOR | VT_UI1  (For variants: VT_ARRAY | VT_UI1)
//  FormatID: (FMTID_ShellDetails) 28636AA6-953D-11D2-B5D6-00C04FD918D0, 2 (PID_DESCRIPTIONID)
//
//  The contents of a SHDESCRIPTIONID structure as a buffer of bytes.
  PKEY_DescriptionID: TPropertyKey = (
      fmtid: (D1: $28636AA6; D2: $953D; D3: $11D2;
      D4: ($B5, $D6, $00, $C0, $4F, $D9, $18, $D0));
      pid: 2);

//  Name:     System.InternalName -- PKEY_InternalName
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSFMTID_VERSION) 0CEF7D53-FA64-11D1-A203-0000F81FEDEE, 5 (PIDVSI_InternalName)
//
//  
  PKEY_InternalName: TPropertyKey = (
      fmtid: (D1: $0CEF7D53; D2: $FA64; D3: $11D1;
      D4: ($A2, $03, $00, $00, $F8, $1F, $ED, $EE));
      pid: 5);

//  Name:     System.Link.TargetSFGAOFlagsStrings -- PKEY_Link_TargetSFGAOFlagsStrings
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: D6942081-D53B-443D-AD47-5E059D9CD27A, 3
//  
//  Expresses the SFGAO flags of a link as string values and is used as a query optimization.  See 
//  PKEY_Shell_SFGAOFlagsStrings for possible values of this.
  PKEY_Link_TargetSFGAOFlagsStrings: TPropertyKey = (
      fmtid: (D1: $D6942081; D2: $D53B; D3: $443D;
      D4: ($AD, $47, $5E, $05, $9D, $9C, $D2, $7A));
      pid: 3);

//  Name:     System.Link.TargetUrl -- PKEY_Link_TargetUrl
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 5CBF2787-48CF-4208-B90E-EE5E5D420294, 2  (PKEYs relating to URLs.  Used by IE History.)
  PKEY_Link_TargetUrl: TPropertyKey = (
      fmtid: (D1: $5CBF2787; D2: $48CF; D3: $4208;
      D4: ($B9, $0E, $EE, $5E, $5D, $42, $02, $94));
      pid: 2);

//  Name:     System.Shell.SFGAOFlagsStrings -- PKEY_Shell_SFGAOFlagsStrings
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: D6942081-D53B-443D-AD47-5E059D9CD27A, 2
//
//  Expresses the SFGAO flags as string values and is used as a query optimization.
  PKEY_Shell_SFGAOFlagsStrings: TPropertyKey = (
      fmtid: (D1: $D6942081; D2: $D53B; D3: $443D;
      D4: ($AD, $47, $5E, $05, $9D, $9C, $D2, $7A));
      pid: 2);

// Possible discrete values for PKEY_Shell_SFGAOFlagsStrings are:
  SFGAOSTR_FILESYS: PWideChar = 'filesys';               // SFGAO_FILESYSTEM
  SFGAOSTR_FILEANC: PWideChar = 'fileanc';               // SFGAO_FILESYSANCESTOR
  SFGAOSTR_STORAGEANC: PWideChar = 'storageanc';               // SFGAO_STORAGEANCESTOR
  SFGAOSTR_STREAM: PWideChar = 'stream';               // SFGAO_STREAM
  SFGAOSTR_LINK: PWideChar = 'link';               // SFGAO_LINK
  SFGAOSTR_HIDDEN: PWideChar = 'hidden';               // SFGAO_HIDDEN
  SFGAOSTR_FOLDER: PWideChar = 'folder';               // SFGAO_FOLDER
  SFGAOSTR_NONENUM: PWideChar = 'nonenum';               // SFGAO_NONENUMERATED
  SFGAOSTR_BROWSABLE: PWideChar = 'browsable';               // SFGAO_BROWSABLE
 
//-----------------------------------------------------------------------------
// Software properties



//  Name:     System.Software.DateLastUsed -- PKEY_Software_DateLastUsed
//  Type:     DateTime -- VT_FILETIME  (For variants: VT_DATE)
//  FormatID: 841E4F90-FF59-4D16-8947-E81BBFFAB36D, 16
//  
//  
  PKEY_Software_DateLastUsed: TPropertyKey = (
      fmtid: (D1: $841E4F90; D2: $FF59; D3: $4D16;
      D4: ($89, $47, $E8, $1B, $BF, $FA, $B3, $6D));
      pid: 16);

//  Name:     System.Software.ProductName -- PKEY_Software_ProductName
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (PSFMTID_VERSION) 0CEF7D53-FA64-11D1-A203-0000F81FEDEE, 7
//  
//  
  PKEY_Software_ProductName: TPropertyKey = (
      fmtid: (D1: $0CEF7D53; D2: $FA64; D3: $11D1;
      D4: ($A2, $03, $00, $00, $F8, $1F, $ED, $EE));
      pid: 7);
 
//-----------------------------------------------------------------------------
// Sync properties



//  Name:     System.Sync.Comments -- PKEY_Sync_Comments
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 7BD5533E-AF15-44DB-B8C8-BD6624E1D032, 13
  PKEY_Sync_Comments: TPropertyKey = (
      fmtid: (D1: $7BD5533E; D2: $AF15; D3: $44DB;
      D4: ($B8, $C8, $BD, $66, $24, $E1, $D0, $32));
      pid: 13);

//  Name:     System.Sync.ConflictDescription -- PKEY_Sync_ConflictDescription
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: CE50C159-2FB8-41FD-BE68-D3E042E274BC, 4
  PKEY_Sync_ConflictDescription: TPropertyKey = (
      fmtid: (D1: $CE50C159; D2: $2FB8; D3: $41FD;
      D4: ($BE, $68, $D3, $E0, $42, $E2, $74, $BC));
      pid: 4);

//  Name:     System.Sync.ConflictFirstLocation -- PKEY_Sync_ConflictFirstLocation
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: CE50C159-2FB8-41FD-BE68-D3E042E274BC, 6
  PKEY_Sync_ConflictFirstLocation: TPropertyKey = (
      fmtid: (D1: $CE50C159; D2: $2FB8; D3: $41FD;
      D4: ($BE, $68, $D3, $E0, $42, $E2, $74, $BC));
      pid: 6);

//  Name:     System.Sync.ConflictSecondLocation -- PKEY_Sync_ConflictSecondLocation
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: CE50C159-2FB8-41FD-BE68-D3E042E274BC, 7
  PKEY_Sync_ConflictSecondLocation: TPropertyKey = (
      fmtid: (D1: $CE50C159; D2: $2FB8; D3: $41FD;
      D4: ($BE, $68, $D3, $E0, $42, $E2, $74, $BC));
      pid: 7);

//  Name:     System.Sync.HandlerCollectionID -- PKEY_Sync_HandlerCollectionID
//  Type:     Guid -- VT_CLSID
//  FormatID: 7BD5533E-AF15-44DB-B8C8-BD6624E1D032, 2
  PKEY_Sync_HandlerCollectionID: TPropertyKey = (
      fmtid: (D1: $7BD5533E; D2: $AF15; D3: $44DB;
      D4: ($B8, $C8, $BD, $66, $24, $E1, $D0, $32));
      pid: 2);

//  Name:     System.Sync.HandlerID -- PKEY_Sync_HandlerID
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 7BD5533E-AF15-44DB-B8C8-BD6624E1D032, 3
  PKEY_Sync_HandlerID: TPropertyKey = (
      fmtid: (D1: $7BD5533E; D2: $AF15; D3: $44DB;
      D4: ($B8, $C8, $BD, $66, $24, $E1, $D0, $32));
      pid: 3);

//  Name:     System.Sync.HandlerName -- PKEY_Sync_HandlerName
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: CE50C159-2FB8-41FD-BE68-D3E042E274BC, 2
  PKEY_Sync_HandlerName: TPropertyKey = (
      fmtid: (D1: $CE50C159; D2: $2FB8; D3: $41FD;
      D4: ($BE, $68, $D3, $E0, $42, $E2, $74, $BC));
      pid: 2);

//  Name:     System.Sync.HandlerType -- PKEY_Sync_HandlerType
//  Type:     UInt32 -- VT_UI4
//  FormatID: 7BD5533E-AF15-44DB-B8C8-BD6624E1D032, 8
//  
//  
  PKEY_Sync_HandlerType: TPropertyKey = (
      fmtid: (D1: $7BD5533E; D2: $AF15; D3: $44DB;
      D4: ($B8, $C8, $BD, $66, $24, $E1, $D0, $32));
      pid: 8);

// Possible discrete values for PKEY_Sync_HandlerType are:
  SYNC_HANDLERTYPE_OTHER = 0;
  SYNC_HANDLERTYPE_PROGRAMS = 1;
  SYNC_HANDLERTYPE_DEVICES = 2;
  SYNC_HANDLERTYPE_FOLDERS = 3;
  SYNC_HANDLERTYPE_WEBSERVICES = 4;
  SYNC_HANDLERTYPE_COMPUTERS = 5;

//  Name:     System.Sync.HandlerTypeLabel -- PKEY_Sync_HandlerTypeLabel
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 7BD5533E-AF15-44DB-B8C8-BD6624E1D032, 9
//  
//  
  PKEY_Sync_HandlerTypeLabel: TPropertyKey = (
      fmtid: (D1: $7BD5533E; D2: $AF15; D3: $44DB;
      D4: ($B8, $C8, $BD, $66, $24, $E1, $D0, $32));
      pid: 9);

//  Name:     System.Sync.ItemID -- PKEY_Sync_ItemID
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 7BD5533E-AF15-44DB-B8C8-BD6624E1D032, 6
  PKEY_Sync_ItemID: TPropertyKey = (
      fmtid: (D1: $7BD5533E; D2: $AF15; D3: $44DB;
      D4: ($B8, $C8, $BD, $66, $24, $E1, $D0, $32));
      pid: 6);

//  Name:     System.Sync.ItemName -- PKEY_Sync_ItemName
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: CE50C159-2FB8-41FD-BE68-D3E042E274BC, 3
  PKEY_Sync_ItemName: TPropertyKey = (
      fmtid: (D1: $CE50C159; D2: $2FB8; D3: $41FD;
      D4: ($BE, $68, $D3, $E0, $42, $E2, $74, $BC));
      pid: 3);
 
//-----------------------------------------------------------------------------
// Task properties

//  Name:     System.Task.BillingInformation -- PKEY_Task_BillingInformation
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: D37D52C6-261C-4303-82B3-08B926AC6F12, 100
  PKEY_Task_BillingInformation: TPropertyKey = (
      fmtid: (D1: $D37D52C6; D2: $261C; D3: $4303;
      D4: ($82, $B3, $08, $B9, $26, $AC, $6F, $12));
      pid: 100);

//  Name:     System.Task.CompletionStatus -- PKEY_Task_CompletionStatus
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 084D8A0A-E6D5-40DE-BF1F-C8820E7C877C, 100
  PKEY_Task_CompletionStatus: TPropertyKey = (
      fmtid: (D1: $084D8A0A; D2: $E6D5; D3: $40DE;
      D4: ($BF, $1F, $C8, $82, $0E, $7C, $87, $7C));
      pid: 100);

//  Name:     System.Task.Owner -- PKEY_Task_Owner
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: 08C7CC5F-60F2-4494-AD75-55E3E0B5ADD0, 100
  PKEY_Task_Owner: TPropertyKey = (
      fmtid: (D1: $08C7CC5F; D2: $60F2; D3: $4494;
      D4: ($AD, $75, $55, $E3, $E0, $B5, $AD, $D0));
      pid: 100);

 
 
//-----------------------------------------------------------------------------
// Video properties

//  Name:     System.Video.Compression -- PKEY_Video_Compression
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_VideoSummaryInformation) 64440491-4C8B-11D1-8B70-080036B11A03, 10 (PIDVSI_COMPRESSION)
//
//  Indicates the level of compression for the video stream.  "Compression".
  PKEY_Video_Compression: TPropertyKey = (
      fmtid: (D1: $64440491; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 10);

//  Name:     System.Video.Director -- PKEY_Video_Director
//  Type:     Multivalue String -- VT_VECTOR | VT_LPWSTR  (For variants: VT_ARRAY | VT_BSTR)
//  FormatID: (PSGUID_MEDIAFILESUMMARYINFORMATION) 64440492-4C8B-11D1-8B70-080036B11A03, 20 (PIDMSI_DIRECTOR)
//
//  
  PKEY_Video_Director: TPropertyKey = (
      fmtid: (D1: $64440492; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 20);

//  Name:     System.Video.EncodingBitrate -- PKEY_Video_EncodingBitrate
//  Type:     UInt32 -- VT_UI4
//  FormatID: (FMTID_VideoSummaryInformation) 64440491-4C8B-11D1-8B70-080036B11A03, 8 (PIDVSI_DATA_RATE)
//
//  Indicates the data rate in "bits per second" for the video stream. "DataRate".
  PKEY_Video_EncodingBitrate: TPropertyKey = (
      fmtid: (D1: $64440491; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 8);

//  Name:     System.Video.FourCC -- PKEY_Video_FourCC
//  Type:     UInt32 -- VT_UI4
//  FormatID: (FMTID_VideoSummaryInformation) 64440491-4C8B-11D1-8B70-080036B11A03, 44
//  
//  Indicates the 4CC for the video stream.
  PKEY_Video_FourCC: TPropertyKey = (
      fmtid: (D1: $64440491; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 44);

//  Name:     System.Video.FrameHeight -- PKEY_Video_FrameHeight
//  Type:     UInt32 -- VT_UI4
//  FormatID: (FMTID_VideoSummaryInformation) 64440491-4C8B-11D1-8B70-080036B11A03, 4
//
//  Indicates the frame height for the video stream.
  PKEY_Video_FrameHeight: TPropertyKey = (
      fmtid: (D1: $64440491; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 4);

//  Name:     System.Video.FrameRate -- PKEY_Video_FrameRate
//  Type:     UInt32 -- VT_UI4
//  FormatID: (FMTID_VideoSummaryInformation) 64440491-4C8B-11D1-8B70-080036B11A03, 6 (PIDVSI_FRAME_RATE)
//
//  Indicates the frame rate in "frames per millisecond" for the video stream.  "FrameRate".
  PKEY_Video_FrameRate: TPropertyKey = (
      fmtid: (D1: $64440491; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 6);

//  Name:     System.Video.FrameWidth -- PKEY_Video_FrameWidth
//  Type:     UInt32 -- VT_UI4
//  FormatID: (FMTID_VideoSummaryInformation) 64440491-4C8B-11D1-8B70-080036B11A03, 3
//
//  Indicates the frame width for the video stream.
  PKEY_Video_FrameWidth: TPropertyKey = (
      fmtid: (D1: $64440491; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 3);

//  Name:     System.Video.HorizontalAspectRatio -- PKEY_Video_HorizontalAspectRatio
//  Type:     UInt32 -- VT_UI4
//  FormatID: (FMTID_VideoSummaryInformation) 64440491-4C8B-11D1-8B70-080036B11A03, 42
//  
//  Indicates the horizontal portion of the aspect ratio. The X portion of XX:YY,
//  like 16:9.
  PKEY_Video_HorizontalAspectRatio: TPropertyKey = (
      fmtid: (D1: $64440491; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 42);

//  Name:     System.Video.SampleSize -- PKEY_Video_SampleSize
//  Type:     UInt32 -- VT_UI4
//  FormatID: (FMTID_VideoSummaryInformation) 64440491-4C8B-11D1-8B70-080036B11A03, 9 (PIDVSI_SAMPLE_SIZE)
//
//  Indicates the sample size in bits for the video stream.  "SampleSize".
  PKEY_Video_SampleSize: TPropertyKey = (
      fmtid: (D1: $64440491; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 9);

//  Name:     System.Video.StreamName -- PKEY_Video_StreamName
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_VideoSummaryInformation) 64440491-4C8B-11D1-8B70-080036B11A03, 2 (PIDVSI_STREAM_NAME)
//
//  Indicates the name for the video stream. "StreamName".
  PKEY_Video_StreamName: TPropertyKey = (
      fmtid: (D1: $64440491; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 2);

//  Name:     System.Video.StreamNumber -- PKEY_Video_StreamNumber
//  Type:     UInt16 -- VT_UI2
//  FormatID: (FMTID_VideoSummaryInformation) 64440491-4C8B-11D1-8B70-080036B11A03, 11 (PIDVSI_STREAM_NUMBER)
//
//  "Stream Number".
  PKEY_Video_StreamNumber: TPropertyKey = (
      fmtid: (D1: $64440491; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 11);

//  Name:     System.Video.TotalBitrate -- PKEY_Video_TotalBitrate
//  Type:     UInt32 -- VT_UI4
//  FormatID: (FMTID_VideoSummaryInformation) 64440491-4C8B-11D1-8B70-080036B11A03, 43 (PIDVSI_TOTAL_BITRATE)
//
//  Indicates the total data rate in "bits per second" for all video and audio streams.
  PKEY_Video_TotalBitrate: TPropertyKey = (
      fmtid: (D1: $64440491; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 43);

//  Name:     System.Video.VerticalAspectRatio -- PKEY_Video_VerticalAspectRatio
//  Type:     UInt32 -- VT_UI4
//  FormatID: (FMTID_VideoSummaryInformation) 64440491-4C8B-11D1-8B70-080036B11A03, 45
//  
//  Indicates the vertical portion of the aspect ratio. The Y portion of 
//  XX:YY, like 16:9.
  PKEY_Video_VerticalAspectRatio: TPropertyKey = (
      fmtid: (D1: $64440491; D2: $4C8B; D3: $11D1;
      D4: ($8B, $70, $08, $00, $36, $B1, $1A, $03));
      pid: 45);

 
 
//-----------------------------------------------------------------------------
// Volume properties

//  Name:     System.Volume.FileSystem -- PKEY_Volume_FileSystem
//  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
//  FormatID: (FMTID_Volume) 9B174B35-40FF-11D2-A27E-00C04FC30871, 4 (PID_VOLUME_FILESYSTEM)  (Filesystem Volume Properties)
//
//  Indicates the filesystem of the volume.
  PKEY_Volume_FileSystem: TPropertyKey = (
      fmtid: (D1: $9B174B35; D2: $40FF; D3: $11D2;
      D4: ($A2, $7E, $00, $C0, $4F, $C3, $08, $71));
      pid: 4);

//  Name:     System.Volume.IsMappedDrive -- PKEY_Volume_IsMappedDrive
//  Type:     Boolean -- VT_BOOL
//  FormatID: 149C0B69-2C2D-48FC-808F-D318D78C4636, 2
  PKEY_Volume_IsMappedDrive: TPropertyKey = (
      fmtid: (D1: $149C0B69; D2: $2C2D; D3: $48FC;
      D4: ($80, $8F, $D3, $18, $D7, $8C, $46, $36));
      pid: 2);

//  Name:     System.Volume.IsRoot -- PKEY_Volume_IsRoot
//  Type:     Boolean -- VT_BOOL
//  FormatID: (FMTID_Volume) 9B174B35-40FF-11D2-A27E-00C04FC30871, 10  (Filesystem Volume Properties)
//
//  
  PKEY_Volume_IsRoot: TPropertyKey = (
      fmtid: (D1: $9B174B35; D2: $40FF; D3: $11D2;
      D4: ($A2, $7E, $00, $C0, $4F, $C3, $08, $71));
      pid: 10);

function IsEqualPropertyKey(const a, b: TPropertyKey): Boolean;

implementation

uses ActiveX;

function IsEqualPropertyKey(const a, b: TPropertyKey): Boolean;
begin
  Result := (a.pid = b.pid) and IsEqualIID(a.fmtid, b.fmtid);
end;

end.



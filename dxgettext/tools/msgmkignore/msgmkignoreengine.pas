unit msgmkignoreengine;

interface

uses
  Classes, SysUtils, poparser, xgettexttools;

type
  Tmsgmkignoreengine=
    class
    public
      inputfilename: string;
      outputfilename: string;
      FileNameBlackList: TFileName;
      FileNameWhiteList: TFileName;
      procedure Execute;
    private
      infile: TextFile;
      outfile: TFileStream;
      procedure HandleEntry ( xEntry: TPoEntry;
                              xBlackList, xWhiteList: TPoEntryList);
    end;
    
implementation

uses
  gnugettext, ignoredetector;

{ Tmsgmkignoreengine }

procedure Tmsgmkignoreengine.Execute;
var
  lPop: TPoParser;
  lEntry: TPoEntry;
  lBlackList, lWhiteList: TPoEntryList;
begin
  if inputfilename='' then
  begin
    raise Exception.Create (_('No input filename specified.'));
  end;

  if outputfilename='' then
  begin
    raise Exception.Create (_('No output filename specified.'));
  end;

  lBlackList := TPoEntryList.Create;
  lWhiteList := TPoEntryList.Create;
  try
    if (FileNameBlackList <> '') and
       FileExists( FileNameBlackList) then
    begin
      lBlackList.LoadFromFile( FileNameBlackList);
    end;

    if (FileNameWhiteList <> '') and
       FileExists( FileNameWhiteList) then
    begin
      lWhiteList.LoadFromFile( FileNameWhiteList);
    end;

    AssignFile( infile,
                inputfilename);
    Reset (infile);

    outfile := TFileStream.Create( outputfilename, fmCreate);
    try
      lPop := TPoParser.Create;
      try
        StreamWrite (outfile, '#  '+_('Ignore list. Remove those that you do want to have translated.'));
        StreamWrite (outfile, sLineBreak);
        StreamWrite (outfile, sLineBreak);

        while true do
        begin
          lEntry := lPop.ReadNextEntry( infile);

          if lEntry = nil then
          begin
            break;
          end;

          HandleEntry( lEntry,
                       lBlackList,
                       lWhiteList);
        end;
      finally
        FreeAndNil( lPop);
      end;

      FormatOutputWithMsgCat( outfile);

    finally
      FreeAndNil( outfile);
      CloseFile( infile);
    end;
  finally
    FreeAndNil( lBlackList);
    FreeAndNil( lWhiteList);
  end;
end;

procedure Tmsgmkignoreengine.HandleEntry( xEntry: TPoEntry;
                                          xBlackList, xWhiteList: TPoEntryList);
begin
  if not IsProbablyTranslatable( xEntry,
                                 xBlackList,
                                 xWhiteList) then
  begin
    xentry.WriteToStream( outfile);
  end;
end;

end.

//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\
// Veloce static site generator
//   console unit
//   create console application
// @author : Zendrael <zendrael@gmail.com>
// @date   : 2013/10/20
//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\
unit untConsoleVeloce;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, CustApp,
    //the Content Management System
    untSSG;

type
	TVeloceConsole = class( TCustomApplication )
	protected
    	{Config: TSetupVeloce;
    	strCatalogFiles, strCatalogTags: TStringList;  }
    	procedure DoRun; override;
	public
		constructor Create(TheOwner: TComponent); override;
		destructor Destroy; override;

        {procedure Draft( strFileName: string );
		procedure Publish;
		procedure Update( strFileName: string );
        }
        procedure Help; virtual;
        {

		function ProcessaTema(): WideString;
		function ProcessaPost( strDetail: string; strPost: TPost; criaLink: boolean): WideString;

		procedure ProcessaArquivo( strNomeArquivo: string; strCaminho: string; bolFinal: Boolean );

		procedure CriaIndexes;
		procedure CriaTags;
        }
		{
		procedure setCatalog( strTipo: string ); //tags ou posts
		procedure getCatalog( strTipo: string ); //tags ou posts
		}

	end;

implementation

procedure TVeloceConsole.DoRun;
var
  ErrorMsg: String;
  SSG : TSSG;
begin
    (*
		╔ = 201
		╚ = 200
		╗ = 187
		╝ = 188
		═ = 205
		║ = 186
	*)
	WriteLn('╔══════════════════════════════════════════════════════════════════════════════╗');
	WriteLn('║   VELOCE                                                                     ║');
	WriteLn('╚══════════════════════════════════════════════════════════════════════════════╝');
    WriteLn(' Static Site and Blog Generator                                           v0.8b ');
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('');
    WriteLn('');

	{
		-d <filename> (draft/rascunho)
		-p (publica todos os posts e gera/atualiza o index, tags, sitemap e feed)
		-u (gera/atualiza o index, tags, sitemap e feed)
		-h (ajuda)
	}

	// quick check parameters
	ErrorMsg:= CheckOptions('dpu:h','draft publish update: help');

    if ErrorMsg<>'' then
    begin
		ShowException( Exception.Create( ErrorMsg ) );
		Terminate;
		Exit;
	end;

	// lendo parâmetros
	if HasOption('c','create') then
    begin
		try
            SSG := TSSG.Create;
            SSG.Draft;//( GetOptionValue('c', 'create') )
		finally
			FreeAndNil( SSG );
		end;

		Terminate;
		Exit;
	end;

	if HasOption('d','draft') then
    begin
		try
            SSG := TSSG.Create;
            SSG.Draft;//( GetOptionValue('d', 'draft') )
		finally
			FreeAndNil( SSG );
		end;

		Terminate;
		Exit;
	end;

	if HasOption('p','publish') then
    begin
		try
            SSG := TSSG.Create;
            SSG.Publish;//( GetOptionValue('p', 'publish') )
		finally
            FreeAndNil( SSG );
		end;

		Terminate;
		Exit;
	end;

	if HasOption('u','update') then
    begin
		{try
			Config:= TSetupVeloce.Create;
			Config.getConfig;

			Atualizar( GetOptionValue('u', 'update') );

		finally
			Config.Destroy;
		end;
        }
		Terminate;
		Exit;
	end;

	if HasOption('h','help') then
    begin
		Help;

		Terminate;
		Exit;
	end;

    //nothing called, show help
    Help;

    //terminate app
	Terminate;
    Exit;
end;

procedure TVeloceConsole.Help;
begin
	// WriteLn('Use as: ',ExeName,' [options]');
	WriteLn('Use as: veloce [options]');
	WriteLn('');
	WriteLn('Options:');
	WriteLn('  -c [foldername]' + #9 + ' Create site folder structure');
	WriteLn('  -d [file]' + #9 + ' Create drafs for all posts or single files');
	WriteLn('  -p' + #9#9 + ' Publish all posts');
	WriteLn('  -u [file]' + #9 + ' Update some file or post');
	WriteLn('  -h, --help' + #9 + ' Show this help message');
	WriteLn('');
	WriteLn('');
end;


constructor TVeloceConsole.Create(TheOwner: TComponent);
begin
	inherited Create(TheOwner);
	StopOnException:=True;
end;

destructor TVeloceConsole.Destroy;
begin
	inherited Destroy;
end;


end.


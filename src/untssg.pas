//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\
// Veloce static site generator
//   Static Site Generator unit
//   create CMS functionalities
// @author : Zendrael <zendrael@gmail.com>
// @date   : 2013/10/20
//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\
unit untSSG;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, math,
    //lets setup it first
    untSettings, untPost;

type
	{ TSSG }

	TSSG = class
		private
			//catalogs
			strMenus: TStringList;
            CatalogIndexes: array of TPost;
            CatalogTags: TStringList;
            intNumPosts : Integer;
		protected
			//
            Config: TSettings;
            Template : TStringList;
		public
			//
			constructor Create;
			destructor Destroy; override;
            //
            procedure Draft;
            procedure Update( strFileName: string );
			procedure Publish;
            //
			procedure ProcessTemplate;
            //function ProcessPage( strDetail: string; strPost: TPost; createLink: boolean): WideString;
			//function ProcessPost( strDetail: string; strPost: TPost; createLink: boolean): WideString;

			procedure ProcessFile( strFileName: string; strPath: string; strLocation: String; strFileType:String; bolFinal: Boolean );

			procedure CreateIndexes;
            function CreatePagination(intPage, intNumPages: Integer): String;
			procedure CreateTagPages;
			{
			procedure setCatalog( strTipo: string ); //tags ou posts
			procedure getCatalog( strTipo: string ); //tags ou posts
			}
	end;


implementation

{ TSSG }

constructor TSSG.Create;
begin
	inherited Create;
end;

destructor TSSG.Destroy;
begin
	inherited Destroy;
end;

procedure TSSG.Draft;
var
	SR: TSearchRec;
begin
    //prepare settings
    Config := TSettings.Create;
	Config.getConfig;

    //get files
	if FindFirst( Config.getPosts() +'*.txt', faAnyFile, SR) = 0 then
	begin
		WriteLn('Criando rascunho de:');
		repeat
			WriteLn( '   ' + SR.Name );
			ProcessFile( SR.Name, Config.getPreview(), Config.getPosts(), 'page', False );
		until FindNext(SR) <> 0;
	end;
	FindClose(SR);

    Config.Destroy;
end;

procedure TSSG.Update(strFileName: string);
begin
	 if strFileName = EmptyStr then
     begin
	 	  //
     end;
end;

procedure TSSG.Publish;
var
	SR: TSearchRec;
begin
    //prepare settings
    Config := TSettings.Create;
	Config.getConfig;

    Template := TStringList.Create;
    ProcessTemplate; //just ONE time

    intNumPosts:= 0; //only on first time, else get from catalog???
    SetLength( CatalogIndexes, 1);

    //get posts
	if FindFirst( Config.getPosts() +'*.txt', faAnyFile, SR) = 0 then
	begin
		WriteLn('Publicando posts:');
		repeat
			WriteLn( '   ' + SR.Name );
			ProcessFile( SR.Name, Config.getFinal(), Config.getPosts(), 'post', True );
            inc( intNumPosts );
            SetLength( CatalogIndexes, intNumPosts+1);
		until FindNext(SR) <> 0;
	end;
	FindClose(SR);

    WriteLn(' ');

    //get pages
	if FindFirst( Config.getPages() +'*.txt', faAnyFile, SR) = 0 then
	begin
		WriteLn('Publicando páginas:');
		repeat
			WriteLn( '   ' + SR.Name );
			ProcessFile( SR.Name, Config.getFinal(), Config.getPages(), 'page', True );
		until FindNext(SR) <> 0;
	end;
	FindClose(SR);

    //generate indexes
    CreateIndexes;

    //generate tags
    //CreateTagPages;

    //free memory
    FreeAndNil( Config );
end;

procedure TSSG.ProcessTemplate;
var
    strTheme, strHeader, strBody, strFooter, strMenu: TStringList;
begin
	 try
         //get all HTML of the main template
	    strTheme := TStringList.Create;
		strTheme.LoadFromFile( Config.getThemes + Config.getThemeName + '/' + 'main.html' );

	    //replace all main blocks
	    strHeader := TStringList.Create;
	    strBody := TStringList.Create;
	    strFooter := TStringList.Create;
	    strMenu := TStringList.Create;

	    strHeader.LoadFromFile( Config.getThemes + Config.getThemeName + '/' + 'header.html' );
	    strBody.LoadFromFile( Config.getThemes + Config.getThemeName + '/' + 'body.html' );
	    strFooter.LoadFromFile( Config.getThemes + Config.getThemeName + '/' + 'footer.html' );
	    strMenu.LoadFromFile( Config.getThemes + Config.getThemeName + '/' + 'menu.html' );

	    //build full page
	    strTheme.Text := StringReplace(strTheme.Text, '%%HEADER%%', strHeader.Text, [rfreplaceAll]);
	    strTheme.Text := StringReplace(strTheme.Text, '%%BODY%%', strBody.Text, [rfreplaceAll]);
	    strTheme.Text := StringReplace(strTheme.Text, '%%FOOTER%%', strFooter.Text, [rfreplaceAll]);
	    strTheme.Text := StringReplace(strTheme.Text, '%%MENUS%%', strMenu.Text, [rfreplaceAll]);

	    //replace page info
		strTheme.Text := StringReplace(strTheme.Text, '%%TITLE%%', Config.getSiteName, [rfreplaceAll]);
	    strTheme.Text := StringReplace(strTheme.Text, '%%DESCRIPTION%%', Config.getSiteDescription, [rfreplaceAll]);
	    strTheme.Text := StringReplace(strTheme.Text, '%%KEYWORDS%%', Config.getSiteKeywords, [rfreplaceAll]);
	    strTheme.Text := StringReplace(strTheme.Text, '%%AUTHOR%%', Config.getSiteAuthor, [rfreplaceAll]);
	    strTheme.Text := StringReplace(strTheme.Text, '%%THEME%%', '../' + Config.getThemes + Config.getThemeName + '/', [rfreplaceAll]);

        //if is created
        Template.Text := strTheme.Text;

     finally
	    FreeAndNil( strTheme );
		FreeAndNil( strHeader );
        FreeAndNil( strBody );
        FreeAndNil( strFooter );
        FreeAndNil( strMenu );
     end;
end;

{function TSSG.ProcessPage(strDetail: string; strPost: TPost; createLink: boolean
	): WideString;
begin

end;

function TSSG.ProcessPost(strDetail: string; strPost: TPost; createLink: boolean
	): WideString;
begin

end;}

procedure TSSG.ProcessFile(strFileName: string; strPath: string; strLocation: String;
	strFileType:String; bolFinal: Boolean);
var
  strPost: TPost;
begin
	try
	    //get full post
	    strPost := TPost.Create;
        strPost.setTemplate( Config.getThemes + Config.getThemeName );
	    strPost.LoadFile( strLocation, strFileName );
        strPost.applyTemplate( Template.Text );
        strPost.save( strPath + strPost.getFileName );
	    //remove old extension and add .html
		//Delete( strFileName, Pos('.', strFileName), Length( strFileName ) );
        CatalogIndexes[intNumPosts] := strPost;
	finally
		//free up memory
        //FreeAndNil( strPost );
	end;

end;

procedure TSSG.CreateIndexes;
var
  page, count, limit, startpost, endpost : Integer;
  index, postlist : TStringList;
begin
	//prepare
    index := TStringList.Create;
    postlist := TStringList.Create;
    limit := StrToInt(Config.getPostsPerPage);
    startpost := 1;
    endpost := limit;

    for page := 1 to ceil( intNumPosts / limit ) do
    begin
      //build base file
      index.Text := Template.Text;
      postlist.Text := EmptyStr;

      //run thru posts
      for count := startpost to endpost do
      begin
      	   postlist.Add( CatalogIndexes[count-1].getSimplePost );
      end;
      startpost := count+1;
      if page+1 < ceil( intNumPosts / limit ) then
      	 endpost := endpost + limit
      else
         endpost:= endpost + (intNumPosts mod limit);

      //replace content
      index.Text := StringReplace(index.Text, '%%POSTS%%', postlist.Text, [rfreplaceAll]);

      //replace PREVIOUS and NEXT page links
      index.Text := StringReplace(index.Text, '%%PAGINATION%%', CreatePagination( page, ceil( intNumPosts / limit ) ), [rfreplaceAll]);

      //save index
      if page = 1 then
         index.SaveToFile( Config.getFinal() + Config.getBlogPage + '.html' ) //'index.html' )
      else
         index.SaveToFile( Config.getFinal() + Config.getBlogPage + '-'+ IntToStr(page) +'.html' );//'index-'+ IntToStr(page) +'.html' );

    end;

    //free up memory
    FreeAndNil( index );
    FreeAndNil( postlist );
end;

function TSSG.CreatePagination(intPage, intNumPages: Integer): String;
var
  strLinks : String;
begin
    strLinks := '<ul>';
    if intPage = 1 then
  	   strLinks := strLinks+'<li><a href="'+ Config.getBlogPage +'-2.html">'+ Config.getPageNext +'</a></li>'
    else if (intPage > 1) and (intPage < intNumPages) then
       if (intPage-1) = 1 then
       	  strLinks := strLinks+'<li><a href="'+ Config.getBlogPage +'.html">'+ Config.getPagePrev +'</a></li><li><a href="'+ Config.getBlogPage +'-'+IntToStr(intPage+1)+'.html">'+ Config.getPageNext +'</a></li>'
       else
          strLinks := strLinks+'<li><a href="'+ Config.getBlogPage +'-'+IntToStr(intPage-1)+'.html">'+ Config.getPagePrev +'</a></li><li><a href="'+ Config.getBlogPage +'-'+IntToStr(intPage+1)+'.html">'+ Config.getPageNext +'</a></li>'
    else
	   if (intPage-1) = 1 then
       	  strLinks := strLinks+'<li><a href="'+ Config.getBlogPage +'.html">'+ Config.getPagePrev +'</a></li>'
       else
          strLinks := strLinks+'<li><a href="'+ Config.getBlogPage +'-'+IntToStr(intPage-1)+'.html">'+ Config.getPagePrev +'</a></li>';

    strLinks := strLinks+'</ul>';
    Result := strLinks;
end;

procedure TSSG.CreateTagPages;
begin

end;

end.


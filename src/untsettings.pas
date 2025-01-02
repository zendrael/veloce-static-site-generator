//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\
// Veloce static site generator
//   settings unit
//   read and write settings
// @author : Zendrael <zendrael@gmail.com>
// @date   : 2013/10/20
//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\
unit untSettings;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, IniFiles;

type

	{ TSettings }

 TSettings = class
	protected
		confFile: TINIFile;
		strThemes: string;
		strPosts: string;
        strPages: string;
		strPreview: string;
		strFinal: string;
		strPostsPerPage: string;
        strPageNext : string;
        strPagePrev : string;
		strThemeName: string;
        booGenerateMenus : boolean;
        strBlogPage : string;
		strSiteAuthor: string;
		strSiteName: string;
		strSiteDescription: string;
		strSiteKeywords: string;
		strDateTime: string;
		strSiteURL: string;
	public
		procedure getConfig;

		procedure setThemes(themePath: string);
		function getThemes: string;

		procedure setPosts(postsPath: string);
		function getPosts: string;

        procedure setPages(pagesPath: string);
		function getPages: string;

		procedure setPreview(previewPath: string);
		function getPreview: string;

		procedure setFinal(finalPath: string);
		function getFinal: string;

		procedure setPostsPerPage(postsPerPage: string);
		function getPostsPerPage: string;

        procedure setPageNext(pageNext: string);
		function getPageNext: string;

        procedure setPagePrev(pagePrev: string);
		function getPagePrev: string;

		procedure setThemeName(themeName: string);
		function getThemeName: string;

        procedure setGenerateMenus(menus: string);
		function getGenerateMenus: Boolean;

        procedure setBlogPage(pageName: string);
		function getBlogPage: string;

		procedure setSiteAuthor(SiteAuthor: string);
		function getSiteAuthor: string;

		procedure setSiteName(SiteName: string);
		function getSiteName: string;

		procedure setSiteDescription(SiteDescription: string);
		function getSiteDescription: string;

		procedure setSiteKeywords(SiteKeywords: string);
		function getSiteKeywords: string;

		procedure setDateTime(SiteDateTime: string);
		function getDateTime: string;

		procedure setSiteURL(strURL: string);
		function getSiteURL: string;

		constructor Create;
		destructor Destroy; override;

	end;

implementation

procedure TSettings.getConfig;
begin
	try
		confFile:= TIniFile.Create('veloce.conf');

		//setup directories
		setPosts( confFile.ReadString('Global','Posts','') );
        setPages( confFile.ReadString('Global','Pages','') );
		setPreview( confFile.ReadString('Global','Preview','') );
		setFinal( confFile.ReadString('Global','Output','') );
		setThemes( confFile.ReadString('Global','Themes','') );

        //setup pages
		setSiteAuthor( confFile.ReadString('Site','Author','') );
		setSiteName( confFile.ReadString('Site','Name','') );
		setSiteDescription( confFile.ReadString('Site','Description','') );
		setSiteKeywords( confFile.ReadString('Site','Keywords','') );
		setThemeName( confFile.ReadString('Site','Theme','') );
        setGenerateMenus( confFile.ReadString('Site','GenerateMenus','') );
        setBlogPage( confFile.ReadString('Site','BlogPage','') );
        setPostsPerPage( confFile.ReadString('Site','PostsPerPage','') );
        setPageNext( confFile.ReadString('Site','PageNext','') );
        setPagePrev( confFile.ReadString('Site','PagePrev','') );
		setDateTime( confFile.ReadString('Site','DateTime','') );
		setSiteURL( confFile.ReadString('Site','SiteURL','') );

	finally
		confFile.Free;
	end;
end;

procedure TSettings.setThemes(themePath: string);
begin
	self.strThemes:= themePath;
end;

function TSettings.getThemes: string;
begin
	Result:= self.strThemes;
end;

procedure TSettings.setPosts(postsPath: string);
begin
	self.strPosts:= postsPath;
end;

function TSettings.getPosts: string;
begin
	Result:= self.strPosts;
end;

procedure TSettings.setPages(pagesPath: string);
begin
    self.strPages:= pagesPath;
end;

function TSettings.getPages: string;
begin
	Result:= self.strPages;
end;

procedure TSettings.setPreview(previewPath: string);
begin
	self.strPreview:= previewPath;
end;

function TSettings.getPreview: string;
begin
	Result:= self.strPreview;
end;

procedure TSettings.setFinal(finalPath: string);
begin
	self.strFinal:= finalPath;
end;

function TSettings.getFinal: string;
begin
	Result:= self.strFinal;
end;

procedure TSettings.setPostsPerPage(postsPerPage: string);
begin
	self.strPostsPerPage:= postsPerPage;
end;

function TSettings.getPostsPerPage: string;
begin
	Result:= self.strPostsPerPage;
end;

procedure TSettings.setPageNext(pageNext: string);
begin
	self.strPageNext:= pageNext;
end;

function TSettings.getPageNext: string;
begin
	Result:= self.strPageNext;
end;

procedure TSettings.setPagePrev(pagePrev: string);
begin
	self.strPagePrev:= pagePrev;
end;

function TSettings.getPagePrev: string;
begin
	Result:= self.strPagePrev;
end;

procedure TSettings.setThemeName(themeName: string);
begin
	self.strThemeName:= themeName;
end;

function TSettings.getThemeName: string;
begin
	Result:= self.strThemeName;
end;

procedure TSettings.setGenerateMenus(menus: string);
begin
    if( menus = 'yes') then
	 self.booGenerateMenus:= true;
end;

function TSettings.getGenerateMenus: Boolean;
begin
	 Result := self.booGenerateMenus;
end;

procedure TSettings.setBlogPage(pageName: string);
begin
	 self.strBlogPage:= pageName;
end;

function TSettings.getBlogPage: string;
begin
	 Result := 	 self.strBlogPage;
end;

procedure TSettings.setSiteAuthor(SiteAuthor: string);
begin
	self.strSiteAuthor:= SiteAuthor;
end;

function TSettings.getSiteAuthor: string;
begin
	Result:= self.strSiteAuthor;
end;

procedure TSettings.setSiteName(SiteName: string);
begin
	self.strSiteName:= SiteName;
end;

function TSettings.getSiteName: string;
begin
	Result:= self.strSiteName;
end;

procedure TSettings.setSiteDescription(SiteDescription: string);
begin
	self.strSiteDescription:= SiteDescription;
end;

function TSettings.getSiteDescription: string;
begin
	Result:= self.strSiteDescription;
end;

procedure TSettings.setSiteKeywords(SiteKeywords: string);
begin
	self.strSiteKeywords:= SiteKeywords;
end;

function TSettings.getSiteKeywords: string;
begin
	Result:= self.strSiteKeywords;
end;

procedure TSettings.setDateTime(SiteDateTime: string);
begin
	self.strDateTime:= SiteDateTime;
end;

function TSettings.getDateTime: string;
begin
	Result:= self.strDateTime;
end;

procedure TSettings.setSiteURL(strURL: string);
begin
	self.strSiteURL:= strURL;
end;

function TSettings.getSiteURL: string;
begin
	Result:= self.strSiteURL;
end;

constructor TSettings.Create;
begin
	//
end;

destructor TSettings.Destroy;
begin
  inherited Destroy;
end;

end.


program veloce;

{$mode objfpc}{$H+}

uses
  SysUtils, veloce_utils, veloce_config, veloce_builder;

const
  VERSION = '1.0.0';

procedure ShowHelp;
begin
  WriteLn('');
  WriteLn('  VELOCE - Static Site Generator v', VERSION);
  WriteLn('  ======================================');
  WriteLn('');
  WriteLn('  Uso: veloce <comando> [opções]');
  WriteLn('');
  WriteLn('  Comandos:');
  WriteLn('    init [nome]   Cria um novo site com estrutura padrão');
  WriteLn('    build         Gera o site em modo produção (pasta dist/)');
  WriteLn('    dev           Gera o site em modo desenvolvimento (pasta dev/)');
  WriteLn('    clean         Remove as pastas dist/ e dev/');
  WriteLn('    help          Mostra esta ajuda');
  WriteLn('');
  WriteLn('  Exemplos:');
  WriteLn('    veloce init meu-blog');
  WriteLn('    cd meu-blog');
  WriteLn('    veloce dev');
  WriteLn('    veloce build');
  WriteLn('');
end;

procedure CmdInit(const SiteName: string);
var
  SiteDir, ContentDir, TemplatesDir, PartialsDir, StaticDir: string;
  ConfigContent, IndexContent, PostContent: string;
  BaseTemplate, HeaderPartial, FooterPartial, NavPartial: string;
  StyleCSS: string;
begin
  if SiteName = '' then
  begin
    WriteLn('Erro: informe o nome do site.');
    WriteLn('Uso: veloce init <nome-do-site>');
    Exit;
  end;

  SiteDir := GetCurrentDir + DirectorySeparator + SiteName;

  if DirectoryExists(SiteDir) then
  begin
    WriteLn('Erro: diretório "', SiteName, '" já existe.');
    Exit;
  end;

  WriteLn('');
  WriteLn('>> Criando novo site: ', SiteName);
  WriteLn('');

  // Criar diretórios
  ContentDir := SiteDir + DirectorySeparator + 'content';
  TemplatesDir := SiteDir + DirectorySeparator + 'templates';
  PartialsDir := TemplatesDir + DirectorySeparator + 'partials';
  StaticDir := SiteDir + DirectorySeparator + 'static';

  ForceDirectories(ContentDir);
  ForceDirectories(ContentDir + DirectorySeparator + 'blog');
  ForceDirectories(PartialsDir);
  ForceDirectories(StaticDir + DirectorySeparator + 'css');
  ForceDirectories(StaticDir + DirectorySeparator + 'images');

  // veloce.toml
  ConfigContent := 'title = "' + SiteName + '"' + LineEnding +
                   'description = "Um site incrível gerado com Veloce"' + LineEnding +
                   'url = "https://exemplo.com"' + LineEnding +
                   'author = "Seu Nome"' + LineEnding +
                   'language = "pt-BR"' + LineEnding;
  StringToFile(SiteDir + DirectorySeparator + 'veloce.toml', ConfigContent);

  // Templates
  HeaderPartial := '<header class="site-header">' + LineEnding +
                   '  <div class="container">' + LineEnding +
                   '    <h1 class="site-title"><a href="/">{{site.title}}</a></h1>' + LineEnding +
                   '    {{> partials/nav.html}}' + LineEnding +
                   '  </div>' + LineEnding +
                   '</header>';
  StringToFile(PartialsDir + DirectorySeparator + 'header.html', HeaderPartial);

  NavPartial := '<nav class="site-nav">' + LineEnding +
                '  <a href="/">Início</a>' + LineEnding +
                '  <a href="/about.html">Sobre</a>' + LineEnding +
                '  <a href="/blog/">Blog</a>' + LineEnding +
                '</nav>';
  StringToFile(PartialsDir + DirectorySeparator + 'nav.html', NavPartial);

  FooterPartial := '<footer class="site-footer">' + LineEnding +
                   '  <div class="container">' + LineEnding +
                   '    <p>&copy; 2026 {{site.title}}. Todos os direitos reservados.</p>' + LineEnding +
                   '  </div>' + LineEnding +
                   '</footer>';
  StringToFile(PartialsDir + DirectorySeparator + 'footer.html', FooterPartial);

  BaseTemplate := '<!DOCTYPE html>' + LineEnding +
                  '<html lang="{{site.language}}">' + LineEnding +
                  '<head>' + LineEnding +
                  '  <meta charset="UTF-8">' + LineEnding +
                  '  <meta name="viewport" content="width=device-width, initial-scale=1.0">' + LineEnding +
                  '  <title>{{title}} | {{site.title}}</title>' + LineEnding +
                  '  <meta name="description" content="{{description}}">' + LineEnding +
                  '  <link rel="stylesheet" href="/css/style.css">' + LineEnding +
                  '</head>' + LineEnding +
                  '<body>' + LineEnding +
                  '  {{> partials/header.html}}' + LineEnding +
                  '  <main class="site-main">' + LineEnding +
                  '    <div class="container">' + LineEnding +
                  '      {{content}}' + LineEnding +
                  '    </div>' + LineEnding +
                  '  </main>' + LineEnding +
                  '  {{> partials/footer.html}}' + LineEnding +
                  '</body>' + LineEnding +
                  '</html>';
  StringToFile(TemplatesDir + DirectorySeparator + 'base.html', BaseTemplate);

  // Conteúdo
  IndexContent := '---' + LineEnding +
                  'title: "Bem-vindo"' + LineEnding +
                  'description: "Página inicial do site"' + LineEnding +
                  '---' + LineEnding +
                  LineEnding +
                  '# Bem-vindo ao ' + SiteName + LineEnding +
                  LineEnding +
                  'Este é um site gerado com **Veloce**, um gerador de sites estáticos rápido e simples.' + LineEnding +
                  LineEnding +
                  '## Recursos' + LineEnding +
                  LineEnding +
                  '- Suporte a **Markdown**' + LineEnding +
                  '- Templates com **partials**' + LineEnding +
                  '- Front matter para metadados' + LineEnding +
                  '- Performance e SEO otimizados' + LineEnding +
                  LineEnding +
                  'Comece editando os arquivos na pasta `content/`!';
  StringToFile(ContentDir + DirectorySeparator + 'index.md', IndexContent);

  PostContent := '---' + LineEnding +
                 'title: "Primeiro Post"' + LineEnding +
                 'description: "Nosso primeiro artigo no blog"' + LineEnding +
                 '---' + LineEnding +
                 LineEnding +
                 '# Primeiro Post' + LineEnding +
                 LineEnding +
                 'Este é o primeiro post do blog! Você pode escrever em **Markdown** e usar todos os recursos do Veloce.' + LineEnding +
                 LineEnding +
                  '```pascal' + LineEnding +
                 'program Hello;' + LineEnding +
                 'begin' + LineEnding +
                 '  WriteLn(''Hello, Veloce!'');' + LineEnding +
                 'end.' + LineEnding +
                 '```' + LineEnding +
                 LineEnding +
                 'Aproveite para criar conteúdo incrível! 🚀';
  StringToFile(ContentDir + DirectorySeparator + 'blog' + DirectorySeparator + 'primeiro-post.md', PostContent);

  // CSS
  StyleCSS := '/* Veloce - Estilos Padrão */' + LineEnding +
              '* { box-sizing: border-box; margin: 0; padding: 0; }' + LineEnding +
              'body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif; line-height: 1.6; color: #333; background: #f8f9fa; }' + LineEnding +
              '.container { max-width: 800px; margin: 0 auto; padding: 0 20px; }' + LineEnding +
              '.site-header { background: #fff; border-bottom: 1px solid #e9ecef; padding: 1rem 0; }' + LineEnding +
              '.site-title a { color: #212529; text-decoration: none; font-size: 1.5rem; font-weight: 700; }' + LineEnding +
              '.site-nav { margin-top: 0.5rem; }' + LineEnding +
              '.site-nav a { color: #495057; text-decoration: none; margin-right: 1.5rem; font-weight: 500; }' + LineEnding +
              '.site-nav a:hover { color: #0d6efd; }' + LineEnding +
              '.site-main { background: #fff; padding: 3rem 0; min-height: 60vh; }' + LineEnding +
              '.site-footer { background: #f8f9fa; border-top: 1px solid #e9ecef; padding: 2rem 0; text-align: center; color: #6c757d; font-size: 0.875rem; }' + LineEnding +
              'h1, h2, h3 { margin-bottom: 1rem; color: #212529; }' + LineEnding +
              'p { margin-bottom: 1rem; }' + LineEnding +
              'a { color: #0d6efd; }' + LineEnding +
              'code { background: #f1f3f5; padding: 0.2rem 0.4rem; border-radius: 4px; font-family: monospace; font-size: 0.9em; }' + LineEnding +
              'pre { background: #f8f9fa; padding: 1rem; border-radius: 8px; overflow-x: auto; margin-bottom: 1rem; }' + LineEnding +
              'pre code { background: none; padding: 0; }' + LineEnding +
              'ul, ol { margin-left: 1.5rem; margin-bottom: 1rem; }' + LineEnding +
              'li { margin-bottom: 0.25rem; }' + LineEnding +
              'hr { border: none; border-top: 1px solid #e9ecef; margin: 2rem 0; }' + LineEnding +
              'img { max-width: 100%; height: auto; border-radius: 8px; }';
  StringToFile(StaticDir + DirectorySeparator + 'css' + DirectorySeparator + 'style.css', StyleCSS);

  WriteLn('  ✓ veloce.toml');
  WriteLn('  ✓ templates/base.html');
  WriteLn('  ✓ templates/partials/header.html');
  WriteLn('  ✓ templates/partials/nav.html');
  WriteLn('  ✓ templates/partials/footer.html');
  WriteLn('  ✓ content/index.md');
  WriteLn('  ✓ content/blog/primeiro-post.md');
  WriteLn('  ✓ static/css/style.css');
  WriteLn('');
  WriteLn('========================================');
  WriteLn('  Site "', SiteName, '" criado com sucesso!');
  WriteLn('');
  WriteLn('  Próximos passos:');
  WriteLn('    cd ', SiteName);
  WriteLn('    veloce dev');
  WriteLn('========================================');
  WriteLn('');
end;

procedure CmdBuild;
begin
  BuildSite(GetCurrentDir, GetCurrentDir + DirectorySeparator + 'dist', False);
end;

procedure CmdDev;
begin
  BuildSite(GetCurrentDir, GetCurrentDir + DirectorySeparator + 'dev', True);
  WriteLn('>> Servidor de desenvolvimento:');
  WriteLn('   Abra o arquivo dev/index.html no navegador');
  WriteLn('   ou use: python -m http.server 8080 --directory dev/');
  WriteLn('');
end;

procedure CmdClean;
var
  DistDir, DevDir: string;
begin
  DistDir := GetCurrentDir + DirectorySeparator + 'dist';
  DevDir := GetCurrentDir + DirectorySeparator + 'dev';

  WriteLn('');
  WriteLn('>> Limpando arquivos gerados...');

  if DirectoryExists(DistDir) then
  begin
    DeleteDir(DistDir);
    WriteLn('  ✓ Removido: dist/');
  end;

  if DirectoryExists(DevDir) then
  begin
    DeleteDir(DevDir);
    WriteLn('  ✓ Removido: dev/');
  end;

  WriteLn('');
  WriteLn('  Limpo!');
  WriteLn('');
end;

var
  Command, Arg: string;
begin
  if ParamCount < 1 then
  begin
    ShowHelp;
    Exit;
  end;

  Command := LowerCase(ParamStr(1));

  if Command = 'init' then
  begin
    if ParamCount >= 2 then
      CmdInit(ParamStr(2))
    else
      CmdInit('');
  end
  else if Command = 'build' then
    CmdBuild
  else if Command = 'dev' then
    CmdDev
  else if Command = 'clean' then
    CmdClean
  else if (Command = 'help') or (Command = '-h') or (Command = '--help') then
    ShowHelp
  else
  begin
    WriteLn('Comando desconhecido: ', Command);
    ShowHelp;
  end;
end.

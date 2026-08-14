# ⚡ Veloce

> Um gerador de sites estático (SSG) rápido, leve e poderoso, escrito em **FreePascal**.

Veloce é ideal para **sites de marketing, blogs e documentação** onde **performance** e **SEO** são prioritários. Ele converte arquivos **Markdown** e **HTML** em páginas estáticas otimizadas, com suporte a **templates**, **partials** e **front matter**.

---

## ✨ Recursos

- 📝 **Markdown nativo** — escreva seu conteúdo em `.md`
- 🎨 **Templates & Partials** — sistema de templates tipo `{{> partials/header.html}}`
- 📋 **Front Matter** — metadados YAML no topo dos arquivos
- ⚡ **Build ultrarrápido** — compilado nativamente em Pascal
- 🧹 **SEO otimizado** — HTML limpo, sem JavaScript desnecessário
- 🖼️ **Assets estáticos** — copia automática de `static/`
- 🏗️ **Zero dependências** — um único executável

---

## 🚀 Instalação

### 1. Compilar

Você precisa do **FreePascal Compiler (fpc)** instalado.

```bash
# Clone ou baixe os arquivos
cd veloce/

# Compilar
fpc -O3 -XX -Xs veloce.pas

# Mover para o PATH
sudo mv veloce /usr/local/bin/
# ou no Windows, adicione ao PATH do sistema
```

### 2. Verificar instalação

```bash
veloce help
```

---

## 📁 Estrutura de um Projeto Veloce

```
meu-site/
├── veloce.toml          # Configuração do site
├── content/             # Arquivos Markdown e HTML
│   ├── index.md
│   ├── about.md
│   └── blog/
│       └── primeiro-post.md
├── templates/           # Templates e partials
│   ├── base.html
│   └── partials/
│       ├── header.html
│       ├── nav.html
│       └── footer.html
├── static/              # Arquivos estáticos (CSS, imagens, fonts)
│   ├── css/
│   │   └── style.css
│   └── images/
├── dist/                # ← Gerado por `veloce build`
└── dev/                 # ← Gerado por `veloce dev`
```

---

## 🛠️ Comandos CLI

| Comando | Descrição |
|---------|-----------|
| `veloce init <nome>` | Cria um novo site com estrutura padrão |
| `veloce post <titulo>` | Cria um novo post em `content/blog/` |
| `veloce build` | Gera o site em `dist/` (modo produção) |
| `veloce dev` | Gera o site em `dev/` (modo desenvolvimento) |
| `veloce clean` | Remove `dist/` e `dev/` |
| `veloce help` | Mostra ajuda |

---

## 📝 Exemplo de Uso

```bash
# Criar um novo blog
veloce init meu-blog
cd meu-blog

# Criar um post
veloce post "Meu segundo post"

# Desenvolver (gera em dev/)
veloce dev

# Abrir no navegador
# Linux/Mac:
python3 -m http.server 8080 --directory dev/
# Windows:
python -m http.server 8080 --directory dev\

# Quando terminar, gerar para produção
veloce build

# O site pronto está em dist/
```

### Páginas vs Posts

- Arquivos em `content/` (fora de `content/blog/`) são tratados como páginas.
- Arquivos em `content/blog/` são tratados como posts e geram URL amigável:
  - `content/blog/meu-post.md` → `dist/blog/meu-post/index.html`
- O índice do blog é gerado automaticamente no build:
  - `dist/blog/index.html` sempre inclui links para todos os posts.
- Paginação automática no índice do blog (10 posts por página por padrão).
- Se existir `templates/post.html`, ele será usado para posts.
- Se existir `templates/page.html`, ele será usado para páginas.
- Se existir `templates/blog.html`, ele será usado para o índice do blog.
- Fallback padrão continua sendo `templates/base.html`.

---

## 📄 Front Matter

Todo arquivo de conteúdo pode ter metadados no topo:

```markdown
---
title: "Meu Artigo Incrível"
description: "Uma descrição para SEO"
---

# Conteúdo começa aqui

Texto em **Markdown** com todos os recursos.
```

Você também pode definir variáveis personalizadas no front matter e reutilizá-las no próprio conteúdo e nos templates:

```markdown
---
title: "Sobre"
description: "Página institucional"
current_date: "10/10/2010"
author_role: "Fundador"
---

Publicado em {{current_date}} por {{author_role}}.
```

---

## 🎨 Templates

### Variáveis disponíveis

| Variável | Descrição |
|----------|-----------|
| `{{title}}` | Título da página (do front matter) |
| `{{description}}` | Descrição da página |
| `{{content}}` | Conteúdo renderizado (Markdown → HTML) |
| `{{site.title}}` | Título do site (veloce.toml) |
| `{{site.description}}` | Descrição do site |
| `{{site.url}}` | URL do site |
| `{{site.author}}` | Autor do site |
| `{{site.language}}` | Idioma do site |

### Partials (includes)

```html
{{> partials/header.html}}
{{> partials/nav.html}}
{{> partials/footer.html}}
```

## 🎭 Temas (Themes)

O Veloce pode trocar de tema apenas substituindo arquivos de `templates/` e `static/css/style.css`.

### Temas de exemplo disponíveis

- `example_templates/orange_dark`
- `example_templates/orange_light`

### Como instalar um tema

Execute na raiz do seu site (ex: `meu-blog/`):

```bash
# Exemplo: instalar orange_dark
cp -R ../example_templates/orange_dark/templates/* templates/
cp -R ../example_templates/orange_dark/templates/partials/* templates/partials/
cp ../example_templates/orange_dark/static/css/style.css static/css/style.css

# Rebuild
../veloce build
```

Para instalar `orange_light`, troque `orange_dark` por `orange_light` nos comandos acima.

### Como trocar de tema depois

1. Copie os arquivos do tema novo para `templates/` e `static/css/style.css`.
2. Rode `veloce build` (ou `../veloce build` se estiver usando o binário local).
3. Abra `dist/` e valide o resultado.

Cada tema já inclui:

- `templates/base.html`
- `templates/post.html`
- `templates/blog.html`
- `templates/partials/*`
- `static/css/style.css`

### Template base padrão

```html
<!DOCTYPE html>
<html lang="{{site.language}}">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>{{title}} | {{site.title}}</title>
  <meta name="description" content="{{description}}">
  <link rel="stylesheet" href="/css/style.css">
</head>
<body>
  {{> partials/header.html}}
  <main class="site-main">
    <div class="container">
      {{content}}
    </div>
  </main>
  {{> partials/footer.html}}
</body>
</html>
```

---

## ⚙️ Configuração (veloce.toml)

```toml
title = "Meu Blog"
description = "Blog sobre tecnologia e programação"
url = "https://meublog.com"
author = "João Silva"
language = "pt-BR"
blog_posts_per_page = 10
```

---

## 🧩 Arquitetura do Código

```
veloce/
├── veloce.pas              # CLI e comandos
├── veloce_utils.pas        # Funções utilitárias (arquivos, diretórios)
├── veloce_config.pas       # Parser de veloce.toml
├── veloce_markdown.pas     # Parser Markdown → HTML
├── veloce_template.pas     # Engine de templates ({{var}}, {{> partial}})
└── veloce_builder.pas      # Orquestração do build
```

---

## 📜 Licença

GPL License — use, modifique e distribua à vontade.

---


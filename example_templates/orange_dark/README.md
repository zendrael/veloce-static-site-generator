# orange_dark

Responsive theme pack for Veloce with a dark-orange visual style.

## Structure

- templates/base.html
- templates/post.html
- templates/blog.html
- templates/partials/header.html
- templates/partials/nav.html
- templates/partials/footer.html
- static/css/style.css

## Apply to a site

From your site root:

```bash
cp -R ../example_templates/orange_dark/templates/* templates/
cp -R ../example_templates/orange_dark/templates/partials/* templates/partials/
cp ../example_templates/orange_dark/static/css/style.css static/css/style.css
```

Then run:

```bash
../veloce build
```

## Quick switch to another theme

You can switch from this theme to `orange_light` by replacing `templates/` and `static/css/style.css`:

```bash
cp -R ../example_templates/orange_light/templates/* templates/
cp -R ../example_templates/orange_light/templates/partials/* templates/partials/
cp ../example_templates/orange_light/static/css/style.css static/css/style.css
```

Then rebuild:

```bash
veloce build
# or, if using the local binary:
../veloce build
```

## Maintenance notes

- Palette, spacing, radii, and shadows are centralized in `:root` CSS variables.
- Keep semantic template files (`base`, `post`, `blog`) separate for easier edits.
- Add any page-specific styles at the bottom of `static/css/style.css`.

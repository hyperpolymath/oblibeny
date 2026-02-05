# Lago Grey Wiki

This directory contains wiki-style documentation for Lago Grey that can be:
1. Published to GitHub Wiki
2. Browsed locally as markdown
3. Used as reference documentation

## Structure

```
wiki/
├── README.md (this file)
├── Home.md                          # Main landing page
├── Ecosystem-Integration.md         # Integration overview
├── Svalinn-Integration.md           # Security layer integration
├── Vordr-Integration.md             # Monitoring integration
├── Selur-Integration.md             # Orchestration integration
└── Cerro-Torre-Integration.md       # Sibling project integration
```

## Publishing to GitHub Wiki

### Option 1: Manual Upload
1. Go to repository wiki: https://github.com/hyperpolymath/oblibeny/wiki
2. Create new pages for each .md file
3. Copy content from wiki/ directory

### Option 2: Git-based (Recommended)
```bash
# Clone the wiki repo
git clone https://github.com/hyperpolymath/oblibeny.wiki.git

# Copy wiki files
cp wiki/*.md oblibeny.wiki/

# Push to wiki
cd oblibeny.wiki
git add .
git commit -m "Add ecosystem integration documentation"
git push
```

### Option 3: Automated (CI/CD)
Add to `.github/workflows/publish-wiki.yml`:
```yaml
name: Publish Wiki
on:
  push:
    paths:
      - 'wiki/**'
jobs:
  publish:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Publish to Wiki
        uses: Andrew-Chen-Wang/github-wiki-action@v4
        with:
          path: wiki/
```

## Local Browsing

### With mdcat
```bash
mdcat wiki/Home.md
mdcat wiki/Ecosystem-Integration.md
```

### With grip (GitHub Flavored Markdown)
```bash
pip install grip
grip wiki/Home.md
# Open http://localhost:6419
```

### With VS Code
1. Install "Markdown Preview Enhanced" extension
2. Open wiki/*.md files
3. Press Ctrl+Shift+V for preview

## Contributing

When adding new integration guides:

1. Create new .md file in wiki/ directory
2. Follow existing format (see Svalinn-Integration.md as template)
3. Add link to Ecosystem-Integration.md
4. Add link to Home.md navigation
5. Include "Back to: [[Ecosystem Integration]]" at bottom

### Template Structure
```markdown
# [Component] Integration

**[Component]** + **Lago Grey** = [Value Proposition]

## Overview
[What this integration provides]

## What [Component] Gets from Lago Grey
[Specific benefits]

## Integration Patterns
[Code examples and patterns]

## [Component]-Specific Features
[Unique features for this integration]

## Examples
[Full code examples]

## Testing Integration
[How to test]

## Next Steps
[Action items]

---

**Back to:** [[Ecosystem Integration]]
```

## Wiki Links

Use double-bracket wiki links for internal navigation:
```markdown
[[Home]]
[[Ecosystem Integration]]
[[Svalinn Integration]]
```

GitHub will automatically convert these to clickable links.

## External Links

Use standard markdown:
```markdown
[GitHub Repo](https://github.com/hyperpolymath/oblibeny)
```

## Images

Store images in `wiki/images/` and reference:
```markdown
![Architecture](images/architecture-diagram.png)
```

## Maintenance

- Update integration guides when APIs change
- Keep code examples synchronized with actual code
- Add new integrations as ecosystem grows
- Review quarterly for accuracy

---

**Questions?** Open an issue: https://github.com/hyperpolymath/oblibeny/issues

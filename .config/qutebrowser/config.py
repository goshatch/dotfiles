config.load_autoconfig()

# Autosave
c.auto_save.session = True

# Prefer dark mode
c.colors.webpage.prefers_color_scheme_dark = True

# View PDFs
c.content.pdfjs = True

# UI font
c.fonts.default_family = 'Iosevka'
c.fonts.default_size = '10pt'

# Theme
config.source('gruvbox-dark-soft.py')

# Password manager
config.bind(',p', 'spawn --userscript qute-bitwarden --totp')
config.bind(',pa', 'spawn --userscript qute-pass')

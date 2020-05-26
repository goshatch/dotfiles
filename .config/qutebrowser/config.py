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

# Save to org
config.bind(',o', 'spawn --userscript qute-capture write -f /home/gueorgui/Dropbox/org/gtd/inbox.org -H inbox')
# config.bind(',o', "jseval location.href='org-protocol://capture?template=c&url='+ encodeURIComponent(location.href) + '&title=' + encodeURIComponent(document.title) + '&body=' + encodeURIComponent(window.getSelection())")

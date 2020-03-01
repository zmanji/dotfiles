settings.startToShowEmoji = 0;
settings.showModeStatus = true
settings.smoothScroll = false

var navSites = /gmail.com|twitter.com/

unmap('j', navSites);
unmap('k', navSites);


// cvim like bindings
map('J', 'E')
map('K', 'R')
map('H', 'S')
map('L', 'D')

unmapAllExcept(['i', 'f', 'J', 'K', '<Esc>'], /messenger.com/)
unmapAllExcept(['i', 'f', 'J', 'K', '<Esc>'], /mail.google.com/)


Hints.style('font-family:system-ui;font-weight: normal;');

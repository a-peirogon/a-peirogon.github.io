(function() {
    document.querySelectorAll('a.footnote-ref').forEach(function(fnref, index) {
        var num = index + 1;
        fnref.id = 'fnref:' + num;
        fnref.hash = '#fn:' + num;
        fnref.classList.add('footnote');
    });

    document.querySelectorAll('section.footnotes li[id^="fn"]').forEach(function(fn, index) {
        var num = index + 1;
        fn.id = 'fn:' + num;
        var backref = fn.querySelector('a.footnote-back');
        if (backref) {
            backref.hash = '#fnref:' + num;
        }
    });

    if (typeof sidenotesSetup === 'function') {
        sidenotesSetup();
    }
})();

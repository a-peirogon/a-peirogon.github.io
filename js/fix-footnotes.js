// Fix footnote IDs to work with sidenotes.js
(function() {
    // Fix footnote reference links
    document.querySelectorAll('a.footnote-ref').forEach(function(fnref, index) {
        var num = index + 1;
        // Change id from "fnref1" to "fnref:1"
        fnref.id = 'fnref:' + num;
        // Change href from "#fn1" to "#fn:1"
        fnref.hash = '#fn:' + num;
        // Add the "footnote" class that sidenotes.js expects
        fnref.classList.add('footnote');
    });

    // Fix footnote definitions
    document.querySelectorAll('section.footnotes li[id^="fn"]').forEach(function(fn, index) {
        var num = index + 1;
        // Change id from "fn1" to "fn:1"
        fn.id = 'fn:' + num;

        // Fix the back-reference link
        var backref = fn.querySelector('a.footnote-back');
        if (backref) {
            backref.hash = '#fnref:' + num;
        }
    });

    // Initialize sidenotes after fixing IDs
    if (typeof sidenotesSetup === 'function') {
        sidenotesSetup();
    }
})();

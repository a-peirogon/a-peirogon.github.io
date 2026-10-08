/* Collapsible boxes (```{.caja}``` blocks): they start collapsed and open or
   close with the "Click to expand" bar. Without JavaScript the content is
   simply always visible. */
(function () {
    function setState(box, expanded) {
        box.classList.toggle("collapsed", !expanded);
        var button = box.querySelector(".portal-box-toggle");
        button.setAttribute("aria-expanded", expanded ? "true" : "false");
        button.querySelector(".label").textContent =
            expanded ? "Click to collapse" : "Click to expand";
    }
    document.querySelectorAll(".portal-box-collapsible").forEach(function (box) {
        var button = box.querySelector(".portal-box-toggle");
        if (!button) return;
        box.classList.add("js-collapsible");
        setState(box, false);
        button.addEventListener("click", function () {
            setState(box, box.classList.contains("collapsed"));
        });
    });
})();

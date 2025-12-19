// Header Anchors - Añade enlaces directos a títulos
(function() {
    'use strict';

    const ANCHOR_SYMBOL = '§';
    const CONTAINER = '#markdownBody';

    function init() {
        const headers = document.querySelectorAll(`${CONTAINER} h1, ${CONTAINER} h2, ${CONTAINER} h3, ${CONTAINER} h4, ${CONTAINER} h5, ${CONTAINER} h6`);

        headers.forEach(header => {
            // Skip si ya tiene ancla
            if (header.querySelector('.header-anchor')) return;

            // Buscar ID (header o parent)
            const id = header.id || (header.parentElement && header.parentElement.id);
            if (!id) return;

            // Crear ancla
            const anchor = document.createElement('a');
            anchor.className = 'header-anchor';
            anchor.href = `#${id}`;
            anchor.textContent = ANCHOR_SYMBOL;
            anchor.title = 'Enlace directo a esta sección';

            // Click handler
            anchor.addEventListener('click', (e) => {
                e.preventDefault();

                // Actualizar URL
                history.replaceState(null, null, `#${id}`);

                // Scroll suave
                document.getElementById(id).scrollIntoView({ behavior: 'smooth' });

                // Copiar al clipboard
                if (navigator.clipboard) {
                    navigator.clipboard.writeText(window.location.href);
                }
            });

            // Insertar ancla AL PRINCIPIO (izquierda)
            header.insertBefore(anchor, header.firstChild);
            header.classList.add('has-anchor');
        });
    }

    // Inicializar
    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', init);
    } else {
        init();
    }
})();

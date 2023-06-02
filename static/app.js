document.addEventListener('DOMContentLoaded', () => {

    // Init sortable.
    document.querySelectorAll('.js-sortable').forEach(function(elem) {
        if (Boolean(elem.jsSortableInitialized) === false) {
            Sortable.create(elem, {
                handle: '.sortable-handle',
                animation: 150,
            });
            elem.jsSortableInitialized = true;
        }
    });

    // Init tinymce
    tinymce.init({
        selector: '.wysiwyg',
        setup: function(editor) {
            editor.on('change', function() {
                tinymce.triggerSave();
            });
        }
    });
});

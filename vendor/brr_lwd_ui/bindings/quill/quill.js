require('quill/dist/quill.core.css')
require('quill/dist/quill.bubble.css')
require('quill/dist/quill.snow.css')

//Provides: get_quill
function get_quill(){
    return require('quill').default
}

//Provides: get_quill_cursors
function get_quill_cursors(){
    return require('quill-cursors')
}

//Provides: get_yjs
function get_yjs() {
    return require('yjs')
}

//Provides: get_web_rtc_provider
function get_web_rtc_provider() {
    return require('y-webrtc').WebrtcProvider
}

//Provides: get_indexeddb_persistence
function get_indexeddb_persistence() {
    return require('y-indexeddb').IndexeddbPersistence
}


//Provides: get_awareness_protocol
function get_awareness_protocol() {
    return require('y-protocols/awareness.js')
}


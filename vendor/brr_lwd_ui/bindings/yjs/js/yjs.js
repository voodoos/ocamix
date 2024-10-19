import * as Y from 'yjs'
import { WebrtcProvider } from 'y-webrtc'
import { IndexeddbPersistence } from 'y-indexeddb'
import { QuillBinding } from 'y-quill'

window.yjs = Y
window.WebrtcProvider = WebrtcProvider
window.IndexeddbPersistence = IndexeddbPersistence
window.QuillBinding = QuillBinding

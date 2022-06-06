import { createApp } from 'vue'
import { createStore } from "vuex";

import App from './App.vue'
import router from './router'

import './index.css'

// Create a new store instance or import from module.
const store = createStore({
    state: {
        paused: false,
    },
    mutations: {
      TOGGLE_PAUSE(state) {
        state.paused = !state.paused;
      },
    },
});

const app = createApp(App)

app.use(router)
app.use(store)

app.mount('#app')

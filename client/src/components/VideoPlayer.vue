<template>
    <div className="video-class">
        <video ref="videoPlayer" class="video-js"></video>
    </div>
    <button v-on:click="toggle()">Pause</button>
    <br />
</template>

<script>
import videojs from 'video.js';
import 'video.js/dist/video-js.min.css';

export default {
    name: 'VideoPlayer',
    props: {
        options: {
            type: Object,
            default() {
                return {};
            }
        }
    },
    data() {
        return {
            player: null
        }
    },
    mounted() {
        this.player = videojs(this.$refs.videoPlayer, this.options, () => {
            this.player.log('onPlayerReady', this);
        });
        console.log("PLAYER", this.player);
    },
    beforeDestroy() {
        if (this.player) {
            this.player.dispose();
        }
    },
    computed: {
        paused() {
            return this.$store.state.paused;
        },
    },
    methods: {
        toggle: function (message) {
            this.$store.commit('TOGGLE_PAUSE')
        }
    },
    watch: {
        paused(val, old) {
            if(val) {
                this.player.pause()
            } else {
                this.player.play()
            }
        }
    }
}
</script>

<style>
video {
    width: 200px;
}
</style>
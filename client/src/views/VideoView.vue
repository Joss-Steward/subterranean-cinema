<template>
	<div>
		<VideoPlayer :options="videoOptions" />
	</div>
</template>

<script>
import VideoPlayer from '@/components/VideoPlayer.vue';

export default {
	name: 'VideoExample',
	components: {
		VideoPlayer
	},
	data() {
		return {
			videoOptions: {
				autoplay: true,
				controls: true,
				fluid: true,
				sources: [
					{
						src: 'http://localhost:8080/media/birdemic.mp4',
						type: 'video/mp4'
					}
				]
			}
		};
	}
};
</script>
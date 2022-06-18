<script setup>
import { RouterLink } from 'vue-router'
</script>

<template>
	<li>
		<router-link :to="{ name: 'item', params: { media_id: this.media_id }}">
		Movie: {{ this.title }}
		</router-link>
	</li>
</template>

<script>
export default {
	props: {
		media_id: String,
		title: String
	}
}
</script>
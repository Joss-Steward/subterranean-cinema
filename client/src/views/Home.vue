<script setup>
import { RouterLink } from 'vue-router'
</script>

<template>
	<RouterLink :to="{ name: 'library' }">
	<h1>Library</h1>
	</RouterLink>
</template>

<script>
export default {
	data() {
		return {}
	}	
}
</script>

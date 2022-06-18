<script setup>
import { RouterLink, RouterView } from 'vue-router'
import TopNavBar from '@/components/TopNavBar.vue'
</script>

<template>
	<div>
		<RouterLink :to="{ name: 'home' }">
			<h1>Home</h1>
		</RouterLink>
	</div>
	<div>
		<RouterView/>
	</div>
</template>

<style>
</style>

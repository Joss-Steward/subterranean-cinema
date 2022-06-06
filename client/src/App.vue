<script setup>
import { RouterLink, RouterView } from 'vue-router'
import TopNavBar from '@/components/TopNavBar.vue'
</script>

<template>
	<div class="bg-slate-800 min-h-screen py-5 space-x-4">
		<div class="flex flex-row">
			<div class="grow flex"></div>
			<RouterLink to="/"
				class="grow-0 mx-auto flex border-slate-200 border-width-2 px-4 sm:px-4 py-2.5 rounded h-14 bg-gradient-to-r from-purple-500 to-pink-500"
				tag="div">
				<div class="font-sans text-2xl text-white">Subterranean Cinema
				</div>
			</RouterLink>
			<div class="grow flex"></div>
		</div>
		<div class="container rounded-md">
			<form>
				<p>Filename</p>
				<input type="text" class="rounded" />
				<p>Room Name</p>
				<input type="text" class="rounded" />
			</form>
		</div>
	</div>
</template>

<style>
</style>

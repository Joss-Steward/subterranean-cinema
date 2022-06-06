<script setup>
import Movies from '@/components/Movies/Movies.vue'
</script>

<template>
	<div class="shadow-md w-1/2">
		<Movies :movies="movies"></Movies>
	</div>
</template>

<script>
export default {
	data() {
		return {
			movies: [
				{
					title: 'A longish yard',
					image: 'http://localhost:8080/media/long.png',
					starRating: 1.2,
					rating: '42%',
					year: 1920,
					genre: 'Prison',
					runtime: '100 Yards',
					cast: 'Mr. Gnome, a rabbit'
				},
				{
					title: 'A longer yard',
					image: 'http://localhost:8080/media/longer.png',
					starRating: 0.8,
					rating: '12%',
					year: 1975,
					genre: 'Mega Prison, Action',
					runtime: '1000 Miles',
					cast: 'Literally none'
				}
			]
		};
	}	
}
</script>

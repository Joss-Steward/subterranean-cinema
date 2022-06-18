<script setup>
import MovieList from '@/components/Media/MovieList.vue'
</script>

<template>
	<div>
		<MovieList :movies="movie_list"></MovieList>
	</div>
</template>

<script>
export default {
	data() {
		return {
			movie_list: ""
		}
	},
	beforeCreate() {
		fetch("http://127.0.0.1:8080/api/media", {
			method: 'GET',
			headers: { 'Content-type': 'application/json' },
		}).then(res => 
			res.json()
		).then((response) => {
            this.movie_list = response;
        }).catch((error) => {
            console.log("error fetching media list:", error)
        });
	}
}
</script>

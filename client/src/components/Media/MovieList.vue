<script setup>
import ListItem from './ListItem.vue'
</script>

<template>
	<div>
		<ul>
		<ListItem v-for="movie in movies" :media_id="movie.id" :title="movie.title" />
		</ul>
	</div>
</template>

<script>
export default {
	props: ['movies'],
	components: {
		ListItem,
	}
};
</script>
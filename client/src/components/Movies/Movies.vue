<template>
	<div class="divide-y divide-slate-100">
		<List>
			<ListItem v-for="movie in movies" :key="movie.id" :movie="movie" />
		</List>
	</div>
</template>

<script>
import List from './List.vue'
import ListItem from './ListItem.vue'

export default {
	props: ['movies'],
	components: {
		List,
		ListItem,
	},
}
</script>
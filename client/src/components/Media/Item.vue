<script setup>
import { RouterLink } from 'vue-router'
</script>

<template>
<div>
	<h2>Title: {{ this.title }}</h2>
	<router-link :to="{ name: 'item', params: { media_id: this.media_id }}">
	<p>Start Session</p>
	</router-link>
</div>
</template>

<script>
export default {
	props: {
		media_id: String
	},
	data() {
		return {
			title: ""
		}
	},
	beforeCreate() {
		fetch("http://127.0.0.1:8080/api/media/" + this.media_id, {
			method: 'GET',
			headers: { 'Content-type': 'application/json' },
		}).then(res => 
			res.json()
		).then((response) => {
            this.title = response;
        }).catch((error) => {
            console.log("error fetching media list:", error)
        });
	}
}
</script>
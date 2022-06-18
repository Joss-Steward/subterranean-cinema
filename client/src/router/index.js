import { createRouter, createWebHistory } from 'vue-router';

import HomeView from '@/views/Home.vue';
import ItemView from '@/views/ItemView.vue';
import Library from '@/views/Library.vue';
import Session from '@/views/Session.vue';

const router = createRouter({
	history: createWebHistory(import.meta.env.BASE_URL),
	routes: [
		{
			path: '/',
			name: 'home',
			component: HomeView
		},
		{
			path: '/library/',
			name: 'library',
			component: Library
		},
		{
			path: '/library/:media_id',
			name: 'item',
			component: ItemView,
			props: true
		},
		{
			path: '/session/:session_id',
			name: 'session',
			component: Session,
			props: true
		}
	]
})

export default router

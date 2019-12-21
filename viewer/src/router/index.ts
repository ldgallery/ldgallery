import Vue from "vue";
import VueRouter from "vue-router";
import Gallery from "@/views/Gallery.vue";

Vue.use(VueRouter);

// async way : component: () => import(/* webpackChunkName: "Gallery" */ "@/views/Gallery.vue"),

const routes = [
  {
    path: "*",
    name: "Gallery",
    component: Gallery,
    props: true
  },
];

const router = new VueRouter({
  routes,
});

export default router;

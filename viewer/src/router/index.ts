import Vue from "vue";
import VueRouter from "vue-router";

Vue.use(VueRouter);

const routes = [
  {
    path: "/",
    name: "root",
    component: () => import(/* webpackChunkName: "root" */ "@/views/Root.vue"),
  },
];

const router = new VueRouter({
  routes,
});

export default router;

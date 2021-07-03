export const MainLayout = () =>
  Promise.all([
    import(/* webpackChunkName: "ui" */ "@/plugins/buefy"),
    import(/* webpackChunkName: "ui" */ "@/components"),
    import(/* webpackChunkName: "ui" */ "@/plugins/lazyimage"),
    import(/* webpackChunkName: "ui" */ "@/plugins/dragscroll"),
    import(/* webpackChunkName: "ui" */ "@/plugins/fontawesome"),
  ]).then(() => import(/* webpackChunkName: "ui" */ "@/views/MainLayout.vue"));

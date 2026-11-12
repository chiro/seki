/* SPDX-License-Identifier: MIT
 * Original seki child-compositor fixture. Uses public wlroots 0.20 APIs.
 * No physical devices or kernel input objects are opened.
 */
#define WLR_USE_UNSTABLE
#include <dlfcn.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <time.h>
#include <xkbcommon/xkbcommon.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <wlr/backend.h>
#include <wlr/interfaces/wlr_keyboard.h>
#include <wlr/interfaces/wlr_pointer.h>
/* The init command may use a host shell with a different libc. Consume the
 * compositor-only preload before any fork/exec; clients receive no backend DSO. */
__attribute__((constructor)) static void consume_preload(void) {
 if (unsetenv("LD_PRELOAD") != 0) abort();
 fprintf(stderr,"fixture backend preload consumed before compositor startup\n");
}
static struct wlr_backend *backend;
static struct wl_event_source *keytimer;
static struct wl_event_source *timer;
static struct wl_event_source *cycle_timer;
static int ack_fd=-1;
static int stage;
static bool active;
static struct wlr_keyboard keyboard;
static struct wlr_keyboard second;
static struct wlr_pointer pointer;
static struct wl_event_source *hotplug_timer;
static bool plugged;
static struct wlr_keyboard hotplug_keyboard;
static struct wlr_pointer hotplug_pointer;
static const struct wlr_pointer_impl pointer_impl = {.name="seki-fixture"};
static const struct wlr_keyboard_impl impl = {0};
static void report(struct wl_listener *listener, void *data) {
 (void)listener; (void)data;
 fprintf(stderr,"fixture repeat observed %d %d\n",keyboard.repeat_info.rate,keyboard.repeat_info.delay);
}
static struct wl_listener repeat = {.notify=report};
static int press(void *data) {
 (void)data;
 if (active) {
  struct wlr_keyboard_key_event e={.time_msec=100,.keycode=42,.update_state=true,.state=WL_KEYBOARD_KEY_STATE_PRESSED};
  wlr_keyboard_notify_key(&keyboard,&e);
  e.keycode=3; wlr_keyboard_notify_key(&keyboard,&e);
  e.state=WL_KEYBOARD_KEY_STATE_RELEASED; wlr_keyboard_notify_key(&keyboard,&e);
  e.keycode=42; wlr_keyboard_notify_key(&keyboard,&e);
  fprintf(stderr,"fixture shifted 2 injected\n");
 }
 return 0;
}
/* With SEKI_FIXTURE_LAYOUT_ACK, a tick changes the layout only when it can
 * take one byte from that FIFO. The reader writes a byte after it has seen
 * the change before, so it sees every change. */
static bool acknowledged(void) {
 char byte;
 return ack_fd<0 || read(ack_fd,&byte,1)==1;
}
static int cycle(void *data) {
 static uint32_t group;
 if (active && acknowledged()) {
  group=1-group;
  struct timespec stamp; clock_gettime(CLOCK_MONOTONIC,&stamp);
  wlr_keyboard_notify_modifiers(&keyboard,0,0,0,group);
  const char *path=getenv("SEKI_FIXTURE_LAYOUT_RECEIPTS");
  if (path) { FILE *file=fopen(path,"a"); if (!file) abort(); fprintf(file,"%lld %u\n",(long long)stamp.tv_sec*1000000000LL+stamp.tv_nsec,group); fclose(file); }
 }
 (void)data; wl_event_source_timer_update(cycle_timer,ack_fd<0 ? 2000 : 10);
 return 0;
}
static int add(void *data) {
 (void)data;
 if (stage==1) {
  wl_list_remove(&repeat.link); wlr_keyboard_finish(&keyboard); wlr_keyboard_finish(&second); wlr_pointer_finish(&pointer); active=false;
  fprintf(stderr,"fixture devices removed\n"); stage=2; wl_event_source_timer_update(timer,300); return 0;
 }
 wlr_keyboard_init(&keyboard,&impl,"seki-private-backend-keyboard");
 wlr_keyboard_init(&second,&impl,"seki-private-second-keyboard");
 struct xkb_context *context=xkb_context_new(XKB_CONTEXT_NO_FLAGS);
 struct xkb_rule_names names={.layout="us,jp"};
 struct xkb_keymap *keymap=xkb_keymap_new_from_names(context,&names,XKB_KEYMAP_COMPILE_NO_FLAGS);
 if (!keymap) abort();
 wlr_keyboard_set_keymap(&keyboard,keymap); wlr_keyboard_set_keymap(&second,keymap);
 xkb_keymap_unref(keymap); xkb_context_unref(context);
 wl_signal_add(&keyboard.events.repeat_info,&repeat);
 wl_signal_emit_mutable(&backend->events.new_input,&second.base);
 wl_signal_emit_mutable(&backend->events.new_input,&keyboard.base);
 wlr_pointer_init(&pointer,&pointer_impl,"seki-private-backend-pointer");
 wl_signal_emit_mutable(&backend->events.new_input,&pointer.base);
 fprintf(stderr,"fixture devices added\n");
 active=true;
 if (stage==0) {stage=1; wl_event_source_timer_update(timer,600);}
 return 0;
}
static int hotplug(void *data) {
 (void)data;
 bool wanted=access(getenv("SEKI_FIXTURE_HOTPLUG"),F_OK)==0;
 if (wanted && !plugged) {
  wlr_keyboard_init(&hotplug_keyboard,&impl,"seki-private-hotplug-keyboard");
  struct xkb_context *context=xkb_context_new(XKB_CONTEXT_NO_FLAGS);
  struct xkb_rule_names names={.layout="us"};
  struct xkb_keymap *keymap=xkb_keymap_new_from_names(context,&names,XKB_KEYMAP_COMPILE_NO_FLAGS);
  if (!keymap) abort();
  wlr_keyboard_set_keymap(&hotplug_keyboard,keymap);
  xkb_keymap_unref(keymap); xkb_context_unref(context);
  wl_signal_emit_mutable(&backend->events.new_input,&hotplug_keyboard.base);
  const char *name=getenv("SEKI_FIXTURE_HOTPLUG_POINTER");
  wlr_pointer_init(&hotplug_pointer,&pointer_impl,name ? name : "seki-private-hotplug-pointer");
  wl_signal_emit_mutable(&backend->events.new_input,&hotplug_pointer.base);
  plugged=true;
  fprintf(stderr,"fixture hotplug devices added\n");
 } else if (!wanted && plugged) {
  wlr_keyboard_finish(&hotplug_keyboard); wlr_pointer_finish(&hotplug_pointer);
  plugged=false;
  fprintf(stderr,"fixture hotplug devices removed\n");
 }
 wl_event_source_timer_update(hotplug_timer,50);
 return 0;
}
struct wlr_backend *wlr_backend_autocreate(struct wl_event_loop *loop, struct wlr_session **session) {
 struct wlr_backend *(*real)(struct wl_event_loop *,struct wlr_session **) = dlsym(RTLD_NEXT,"wlr_backend_autocreate");
 backend=real(loop,session);
 keytimer=wl_event_loop_add_timer(loop,press,NULL);
 wl_event_source_timer_update(keytimer,1800);
 if (getenv("SEKI_FIXTURE_LAYOUT_CYCLE")) {
  const char *ack=getenv("SEKI_FIXTURE_LAYOUT_ACK");
  if (ack && (mkfifo(ack,0600)!=0 || (ack_fd=open(ack,O_RDONLY|O_NONBLOCK|O_CLOEXEC))<0)) abort();
  cycle_timer=wl_event_loop_add_timer(loop,cycle,NULL);
  wl_event_source_timer_update(cycle_timer,2000);
 }
 timer=wl_event_loop_add_timer(loop,add,NULL);
 wl_event_source_timer_update(timer,300);
 if (getenv("SEKI_FIXTURE_HOTPLUG")) {
  hotplug_timer=wl_event_loop_add_timer(loop,hotplug,NULL);
  wl_event_source_timer_update(hotplug_timer,50);
 }
 return backend;
}
void wlr_backend_destroy(struct wlr_backend *b) {
 void (*real)(struct wlr_backend *) = dlsym(RTLD_NEXT,"wlr_backend_destroy");
 if (b==backend) {
  struct wl_event_source **sources[]={&keytimer,&timer,&cycle_timer,&hotplug_timer};
  for (size_t i=0;i<sizeof sources/sizeof *sources;i++) if (*sources[i]) {wl_event_source_remove(*sources[i]); *sources[i]=NULL;}
 }
 if (b==backend && active) {wl_list_remove(&repeat.link); wlr_keyboard_finish(&keyboard); wlr_keyboard_finish(&second); wlr_pointer_finish(&pointer);}
 if (b==backend && plugged) {wlr_keyboard_finish(&hotplug_keyboard); wlr_pointer_finish(&hotplug_pointer); plugged=false;}
 real(b);
}
void wlr_keyboard_set_repeat_info(struct wlr_keyboard *kb, int32_t rate, int32_t delay) {
 void (*real)(struct wlr_keyboard *,int32_t,int32_t) = dlsym(RTLD_NEXT,"wlr_keyboard_set_repeat_info");
 real(kb,rate,delay);
 fprintf(stderr,"fixture group repeat observed %d %d\n",kb->repeat_info.rate,kb->repeat_info.delay);
}
#include <string.h>
struct wl_global *wl_global_create(struct wl_display *display, const struct wl_interface *interface, int version, void *data, wl_global_bind_func_t bind) {
 struct wl_global *(*real)(struct wl_display *,const struct wl_interface *,int,void *,wl_global_bind_func_t)=dlsym(RTLD_NEXT,"wl_global_create");
 if (getenv("SEKI_FIXTURE_NO_INPUT") && (strcmp(interface->name,"river_input_manager_v1")==0 || strcmp(interface->name,"river_libinput_config_v1")==0 || strcmp(interface->name,"river_xkb_config_v1")==0)) {
  static struct wl_interface hidden[3];
  int index=strcmp(interface->name,"river_input_manager_v1")==0 ? 0 : strcmp(interface->name,"river_libinput_config_v1")==0 ? 1 : 2;
  hidden[index]=*interface;
  hidden[index].name=index==0 ? "seki_hidden_input" : index==1 ? "seki_hidden_libinput" : "seki_hidden_xkb";
  interface=&hidden[index];
 }
 if (getenv("SEKI_FIXTURE_INPUT_V1") && strcmp(interface->name,"river_input_manager_v1")==0) version=1;
 if (getenv("SEKI_FIXTURE_XKB_VERSION") && strcmp(interface->name,"river_xkb_config_v1")==0) version=atoi(getenv("SEKI_FIXTURE_XKB_VERSION"));
 return real(display,interface,version,data,bind);
}

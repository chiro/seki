/* SPDX-License-Identifier: MIT
 * Generate test data with the closure's libxkbcommon and xkeyboard-config.
 * With no arguments it writes the US/Japanese pair; with RULES MODEL LAYOUT
 * OPTIONS it writes the keymap those XKB names select.
 */
#include <stdio.h>
#include <stdlib.h>
#include <xkbcommon/xkbcommon.h>
int main(int argc, char **argv) {
 if (argc != 1 && argc != 5) return 2;
 struct xkb_context *context=xkb_context_new(XKB_CONTEXT_NO_FLAGS);
 struct xkb_rule_names names={.layout="us,jp"};
 if (argc == 5) names=(struct xkb_rule_names){.rules=argv[1],.model=argv[2],.layout=argv[3],.options=argv[4]};
 struct xkb_keymap *keymap=xkb_keymap_new_from_names(context,&names,XKB_KEYMAP_COMPILE_NO_FLAGS);
 if (!keymap) return 1;
 char *text=xkb_keymap_get_as_string(keymap,XKB_KEYMAP_FORMAT_TEXT_V1);
 if (!text) return 1;
 int result=fputs(text,stdout)<0;
 free(text); xkb_keymap_unref(keymap); xkb_context_unref(context);
 return result;
}

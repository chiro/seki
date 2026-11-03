#!/usr/bin/env python3
"""Public example compatibility, deterministic summaries, and Pango escaping."""
import importlib.util
from pathlib import Path
import unittest

spec = importlib.util.spec_from_file_location('seki_bar', Path(__file__).with_name('seki.py'))
bar = importlib.util.module_from_spec(spec)
spec.loader.exec_module(bar)


class BarTest(unittest.TestCase):
    def test_older_snapshot(self):
        self.assertEqual(bar.module({'workspaces': []})['text'], '')
        self.assertEqual(bar.module({'workspaces': [], 'xkb': None})['text'], '')

    def test_named_devices_are_sorted_and_escaped(self):
        snapshot = {'focused_title': '<title>&', 'xkb': {'items': [
            {'name': 'z', 'state': {}},
            {'name': '<kbd>', 'state': {'layout_name': '<JP>&'}}], 'truncated': True}}
        text = bar.module(snapshot)['text']
        self.assertEqual(text, '&lt;title&gt;&amp; | &lt;kbd&gt;: &lt;JP&gt;&amp;; z: ? …')

    def test_tiling_and_keyboard_layouts_are_separate(self):
        result = bar.module({'workspaces': [{'focused': True, 'tag': '1', 'layout': 'Tall'}],
                             'xkb': {'items': [{'name': 'kbd', 'state': {'layout_name': 'Japanese'}}]}})
        self.assertEqual(result['text'], '1 Tall | kbd: Japanese')


if __name__ == '__main__':
    unittest.main()

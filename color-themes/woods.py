# -*- coding: utf-8 -*-
"""
  sea style drop in pygments-style directory

    pygments.styles.trac
    ~~~~~~~~~~~~~~~~~~~~

    Port of the default trac highlighter design.

    :copyright: Copyright 2006-2010 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.style import Style
from pygments.token import Keyword, Name, Comment, String, Error, \
     Number, Operator, Generic, Whitespace


class WoodsStyle(Style):
    """
    Port of the default trac highlighter design.
    """

    default_style = ''

    styles = {
        Whitespace:             '#bbbbbb',
        Comment:                '#2149B1',
        Comment.Preproc:        'bold noitalic #000000',
        Comment.Special:        'bold #000000',

        Operator:               'bold #A2590E',

        String:                 '#000000',
        String.Regex:           '#000000',

        Number:                 '#000000',

        Keyword:                'bold #A2590E',
        Keyword.Type:           '#2E8B57',

        Name.Builtin:           '#A2590E',
        Name.Function:          '#000000',
        Name.Class:             'bold #445588',
        Name.Exception:         'bold #990000',
        Name.Namespace:         'bold #2E8B57',
        Name.Variable:          '#000000',
        Name.Constant:          '#000000',
        Name.Tag:               '#000000',
        Name.Attribute:         '#008000',
        Name.Entity:            '#000000',

        Generic.Heading:        '#000000',
        Generic.Subheading:     '#000000',
        Generic.Deleted:        '#000000',
        Generic.Inserted:       '#000000',
        Generic.Error:          '#aa0000',
        Generic.Emph:           'italic',
        Generic.Strong:         'bold',
        Generic.Prompt:         '#000000',
        Generic.Output:         '#000000',
        Generic.Traceback:      '#aa0000',

        Error:                  'bold #2E8B57'
    }

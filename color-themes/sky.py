# -*- coding: utf-8 -*-
"""
    pygments.styles.sky
    ~~~~~~~~~~~~~~~~~~~~~~

    Style similar to the `pastie`_ default style.

    .. _pastie: http://pastie.caboo.se/

    :copyright: Copyright 2006-2010 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.style import Style
from pygments.token import Keyword, Name, Comment, String, Error, \
     Number, Operator, Generic, Whitespace


class SkyStyle(Style):
    """
    Style similar to the pastie default style.
    """

    default_style = ''

    styles = {
        Whitespace:             '#bbbbbb',
        Comment:                '#2149B1',  

        String:                 '#000000',

        Operator:				'#1E3B80',

        Keyword:                'bold #1E3B80',
        Keyword.Pseudo:         'nobold',
        Keyword.Type:           '#0066bb',

        Name.Class:             'bold #bb0066',
        Name.Exception:         'bold #bb0066',
        Name.Function:          '#000000',
        Name.Property:          'bold #336699',
        Name.Namespace:         'bold #0066bb',
        Name.Builtin:           '#1E3B80',
        Name.Variable:          '#336699',
        Name.Variable.Class:    '#336699',
        Name.Variable.Instance: '#3333bb',
        Name.Variable.Global:   '#dd7700',
        Name.Constant:          'bold #003366',
        Name.Tag:               'bold #bb0066',
        Name.Attribute:         '#336699',
        Name.Decorator:         '#555555',
        Name.Label:             'italic #336699',

        Number:                 'bold #0000DD',

        Generic.Heading:        '#333',
        Generic.Subheading:     '#666',
        Generic.Deleted:        'bg:#ffdddd #000000',
        Generic.Inserted:       'bg:#ddffdd #000000',
        Generic.Error:          '#aa0000',
        Generic.Emph:           'italic',
        Generic.Strong:         'bold',
        Generic.Prompt:         '#555555',
        Generic.Output:         '#888888',
        Generic.Traceback:      '#aa0000',

        Error:                  'bold #0066bb'
    }

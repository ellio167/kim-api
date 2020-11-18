#
# CDDL HEADER START
#
# The contents of this file are subject to the terms of the Common Development
# and Distribution License Version 1.0 (the "License").
#
# You can obtain a copy of the license at
# http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
# specific language governing permissions and limitations under the License.
#
# When distributing Covered Code, include this CDDL HEADER in each file and
# include the License file in a prominent location with the name LICENSE.CDDL.
# If applicable, add the following below this CDDL HEADER, with the fields
# enclosed by brackets "[]" replaced with your own identifying information:
#
# Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
#
# CDDL HEADER END
#

#
# Copyright (c) 2013--2020, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Ryan S. Elliott
#

#
# Release: This file is part of the kim-api-2.2.0 package.
#

import subprocess

analyticsScript = '''\
<!-- Global site tag (gtag.js) - Google Analytics -->
<script async
src="https://www.googletagmanager.com/gtag/js?id=UA-25481110-7"></script>
<script>
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

  gtag('config', 'UA-25481110-7');
</script>
'''

script=open('script.html', 'w')
script.write(analyticsScript)
script.close()

subprocess.call('conda install -y cmake', shell=True)
subprocess.call('conda env export -n root', shell=True)
subprocess.call('doxygen -w html orig_header.html orig_footer.html orig_stylesheet.css', shell=True)
subprocess.call('cat script.html orig_footer.html > footer.html', shell=True)
subprocess.call('rm -rf build; mkdir build; cd build; FC=true cmake ../../../ -DREADTHEDOCS=ON; make docs', shell=True)
html_extra_path = ['./build/docs/html']


################################################################################

project = u'kim-api'
author = u'Ryan S. Elliott'

# The suffix(es) of source filenames.
source_suffix = '.rst'

# The master toctree document.
master_doc = 'index'

# The language for content autogenerated by Sphinx.
language = None

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = [u'_build', 'Thumbs.db', '.DS_Store']

# The name of the Pygments (syntax highlighting) style to use.
pygments_style = None

Notes on compiling Graphics32 help:

Check the project and the copyright holder's names in BodySection.tmpl.

The Source Folder contains the raw HTML pages for the project (not Graphics32 PAS file :)). 
The Docs folder contains the finished HTML help. This folder is rebuilt every time 'Transform' is clicked in DocProcessor. Because of this, there's little sense editing files here. All HTML editing should be done in the Source folder.

Scripts/menu_data.js may need manual editing if the compiled menus  don't look right.

In the DocCompiler:

1. Parse Missing PAS Units
DocProcessor's 'Parse Missing PAS Units' button will prompt for a selection of files to add, either as part of the main library, or as additional units separate from the main library. If you happen to choose PAS units that have previously been imported into /Source, it's likely that these existing HTML files will be overwritten - so be careful! Anyhow, even though these new HTML files will need manual editing, they make a very handy start to their documentation. Where possible, comments will also be glean from the source code. Comments will be attributed to the definition immediately below the comment, unless a blank line is encountered in which case the comment is ignored. Trailing comments (ie at the end of lines of code) will also be ignored. Extensive comments can imported by using <include src="filename"> within a comment. Include files must reside in the same folder as the code. Image files can be flagged using <img src="filename"> syntax, though images will have to be manually copied to the /Images folder.

Anything with a hyperlink will also have an entry in 'See Also', in the finished 'Transformed' HTML in /Docs. To force an addition to 'See Also' without referencing the link elsewhere in the webpage, use the following template - <p id="Hidden"><a href="YOUR_REF.htm"> <a href="YOUR_2ND_REF.htm"></a></p>.

Use the following markup to include Delphi code samples:

  <div class="Code"><pre class="brush: delphi;">
  //then add your code here, preferably without a trailing newline char.;</pre></div>


2. The "Transform HTML" button converts the bare-bones HTML in /Source into properly linked and formatted HTML, complete with menus, JavaScripted code etc and
the result will appear in /Docs'.

Also, 'Transform' relies on the meta tag 'Order' in Source/_Body.htm to specify the order files are listed in CHM Contents.

3. Finally, "Compile CHM" converts the HTML in 'Docs' into a CHM file.


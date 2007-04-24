<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
	version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns="http://www.w3.org/1999/xhtml">

<!-- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Object oriented extension to Prolog
%  Release 2.30.0
%
%  Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-->

<xsl:output
	method="xml"
	indent="yes"
	encoding="utf-8"
	omit-xml-declaration="no"
	standalone="no"
	doctype-public="-//W3C//DTD XHTML 1.1//EN"
	doctype-system="http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd"/>


<xsl:template match="/">
	<xsl:processing-instruction name="xml-stylesheet">href="logtalk.css" type="text/css"</xsl:processing-instruction>

	<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
	<head>
        <meta http-equiv="content-type" content="text/html; charset=utf-8" />
		<title><xsl:value-of select="logtalk/entity/name" /></title>
		<link rel="stylesheet" href="logtalk.css" type="text/css" />
	</head>
	<body>
		<div class="header">
			<p class="type"><xsl:value-of select="logtalk/entity/type" /></p>
			<h1 class="code"><xsl:value-of select="logtalk/entity/name" /></h1>
			<xsl:if test="logtalk/entity/comment or logtalk/entity/parameters">
			<blockquote>
			<xsl:if test="logtalk/entity/comment">
				<p class="comment"><xsl:value-of select="logtalk/entity/comment" /></p>
			</xsl:if>
			<xsl:if test="logtalk/entity/parameters">
				<ul class="parameters">
				<xsl:for-each select="logtalk/entity/parameters/parameter">
					<li><code><xsl:value-of select="name" /></code><xsl:text disable-output-escaping="yes"> &amp;ndash; </xsl:text><span class="comment"><xsl:value-of select="description" /></span></li>
				</xsl:for-each>
				</ul>
			</xsl:if>
			</blockquote>
			</xsl:if>
		</div>
		<div class="entity">
			<div class="section">
				<xsl:apply-templates select="logtalk/entity" />
				<xsl:apply-templates select="logtalk/relations" />
			</div>
		</div>
		<div class="predicates">
			<xsl:apply-templates select="logtalk/predicates" />
		</div>
		<div class="remarks">
			<xsl:apply-templates select="logtalk/remarks" />
		</div>
	</body>
	</html>
</xsl:template>


<xsl:template match="logtalk/entity">
	<xsl:if test="author or version or date">
	<dl class="properties">
	<xsl:if test="author">
		<dt class ="key">author:</dt>
			<dd class="value"><code><xsl:value-of select="author" /></code></dd>
	</xsl:if>
	<xsl:if test="version">
		<dt class ="key">version:</dt>
			<dd class="value"><code><xsl:value-of select="version" /></code></dd>
	</xsl:if>
	<xsl:if test="date">
		<dt class ="key">date:</dt>
			<dd class="value"><code><xsl:value-of select="date" /></code></dd>
	</xsl:if>
	<xsl:if test="copyright">
		<dt class ="key">copyright:</dt>
			<dd class="value"><code><xsl:value-of select="copyright" /></code></dd>
	</xsl:if>
	<xsl:if test="license">
		<dt class ="key">license:</dt>
			<dd class="value"><code><xsl:value-of select="license" /></code></dd>
	</xsl:if>
	</dl>
	</xsl:if>
	<dl class="properties">
		<dt class ="key">compilation:</dt>
			<dd class ="value"><code><xsl:value-of select="compilation" /></code></dd>
	</dl>
	<xsl:if test="info">
		<dl class="properties">
		<xsl:for-each select="info">
			<dt class ="key"><xsl:value-of select="key" />:</dt>
				<dd class ="value"><code><xsl:value-of select="value" /></code></dd>
		</xsl:for-each>
		</dl>
	</xsl:if>
</xsl:template>


<xsl:template match="logtalk/relations">
	<xsl:choose>
		<xsl:when test="*">
		<dl class="relations">
			<xsl:if test="implements">
			<dt class ="key">implements:</dt>
				<xsl:apply-templates select="implements" />
			</xsl:if>
			<xsl:if test="imports">
			<dt class ="key">imports:</dt>
				<xsl:apply-templates select="imports" />
			</xsl:if>
			<xsl:if test="extends">
			<dt class ="key">extends:</dt>
				<xsl:apply-templates select="extends" />
			</xsl:if>
			<xsl:if test="instantiates">
			<dt class ="key">instantiates:</dt>
				<xsl:apply-templates select="instantiates" />
			</xsl:if>
			<xsl:if test="specializes">
			<dt class ="key">specializes:</dt>
				<xsl:apply-templates select="specializes" />
			</xsl:if>
			<xsl:if test="uses">
			<dt class ="key">uses:</dt>
				<xsl:apply-templates select="uses" />
			</xsl:if>
			<xsl:if test="calls">
			<dt class ="key">calls:</dt>
				<xsl:apply-templates select="calls" />
			</xsl:if>
		</dl>
		</xsl:when>
		<xsl:otherwise>	
			<p class="comment">(no dependencies on other files)</p>
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>


<xsl:template match="logtalk/relations/uses">
	<dd class ="value"><code><a href="{file}.html"><xsl:value-of select="name" /></a></code></dd>
</xsl:template>


<xsl:template match="logtalk/relations/calls">
	<dd class ="value"><code><a href="{file}.html"><xsl:value-of select="name" /></a></code></dd>
</xsl:template>


<xsl:template match="logtalk/relations/*">
	<dd class ="value"><code><xsl:value-of select="scope" /><xsl:text> </xsl:text><a href="{file}.html"><xsl:value-of select="name" /></a></code></dd>
</xsl:template>


<xsl:template match="logtalk/predicates">
	<div class="public">
	<h2>Public interface</h2>
	<xsl:choose>
		<xsl:when test="public/predicate">
			<xsl:apply-templates select="public/predicate" />
		</xsl:when>
		<xsl:when test="/logtalk/relations/*">		
			<div class="section">
				<p class="comment">(see related entities)</p>
			</div>
		</xsl:when>
		<xsl:otherwise>
			<div class="section">
				<p class="comment">(none)</p>
			</div>
		</xsl:otherwise>
	</xsl:choose>
	</div>
	<div class="protected">
	<h2>Protected interface</h2>
	<xsl:choose>
		<xsl:when test="protected/predicate">
			<xsl:apply-templates select="protected/predicate" />
		</xsl:when>
		<xsl:when test="/logtalk/relations/*">		
			<div class="section">
				<p class="comment">(see related entities)</p>
			</div>
		</xsl:when>
		<xsl:otherwise>
			<div class="section">
				<p class="comment">(none)</p>
			</div>
		</xsl:otherwise>
	</xsl:choose>
	</div>
	<div class="private">
	<h2>Private predicates</h2>
	<xsl:choose>
		<xsl:when test="private/predicate">
			<xsl:apply-templates select="private/predicate" />
		</xsl:when>
		<xsl:when test="/logtalk/relations/*">		
			<div class="section">
				<p class="comment">(see related entities)</p>
			</div>
		</xsl:when>
		<xsl:otherwise>
			<div class="section">
				<p class="comment">(none)</p>
			</div>
		</xsl:otherwise>
	</xsl:choose>
	</div>
</xsl:template>


<xsl:template match="*/predicate">
	<div class="section">
	<h3 class="code"><xsl:value-of select="name" /></h3>
	<xsl:if test="comment">
	<blockquote>
		<p class="comment"><xsl:value-of select="comment" /></p>
	</blockquote>
	</xsl:if>
	<dl class="properties">
		<dt class ="key">compilation:</dt>
			<dd class ="value"><code><xsl:value-of select="compilation" /></code></dd>
		<xsl:if test="template">
		<dt class ="key">template:</dt>
			<dd class ="value"><code><xsl:value-of select="template" /></code></dd>
		</xsl:if>
		<xsl:if test="arguments">
			<dd class ="value"><ul class="arguments">
			<xsl:for-each select="arguments/argument">
				<li><code><xsl:value-of select="name" /></code><xsl:text disable-output-escaping="yes"> &amp;ndash; </xsl:text><span class="comment"><xsl:value-of select="description" /></span></li>
			</xsl:for-each></ul></dd>
		</xsl:if>
		<xsl:if test="meta">
		<dt class ="key">metapredicate template:</dt>
			<dd class ="value"><code><xsl:value-of select="meta" /></code></dd>
		</xsl:if>
		<xsl:if test="mode">
		<dt class ="key">mode<xsl:text disable-output-escaping="yes"> &amp;ndash; </xsl:text>number of solutions:</dt>
		<xsl:for-each select="mode">
			<dd class ="value"><code><xsl:value-of select="template" /><xsl:text disable-output-escaping="yes"> &amp;ndash; </xsl:text><xsl:value-of select="solutions" /></code></dd>
		</xsl:for-each>
		</xsl:if>
		<xsl:if test="exceptions">
		<dt class ="key">exceptions:</dt>
		<xsl:for-each select="exceptions/exception">
			<dd class ="value"><xsl:value-of select="condition" />: <code><xsl:value-of select="term" /></code></dd>
		</xsl:for-each>
		</xsl:if>
		<xsl:if test="examples">
		<dt class ="key">examples:</dt>
			<xsl:for-each select="examples/example">
			<dd class ="value"><dl class="examples">
				<dt class="comment"><xsl:value-of select="description" /></dt>
					<dd class="code"><xsl:value-of select="call" /></dd>
					<dd class="code"><xsl:value-of select="bindings" /></dd>
			</dl></dd>
			</xsl:for-each>
		</xsl:if>
	</dl>
	<xsl:if test="info">
		<dl class="properties">
			<xsl:for-each select="info">
			<dt class ="key"><xsl:value-of select="key" />:</dt>
				<dd class ="value"><code><xsl:value-of select="value" /></code></dd>
			</xsl:for-each>
		</dl>
	</xsl:if>
	</div>
</xsl:template>


<xsl:template match="logtalk/remarks">
	<h2>Remarks</h2>
	<div class="section">
	<xsl:choose>
		<xsl:when test="remark">
			<xsl:apply-templates select="remark" />
		</xsl:when>
		<xsl:otherwise>
			<h3 class="code">(none)</h3>
		</xsl:otherwise>
	</xsl:choose>
	</div>
</xsl:template>

<xsl:template match="logtalk/remarks/remark">
	<dl class="remarks">
		<dt class="comment"><xsl:value-of select="topic" /></dt>
			<dd class="text"><xsl:value-of select="text" /></dd>
	</dl>
</xsl:template>


</xsl:stylesheet>

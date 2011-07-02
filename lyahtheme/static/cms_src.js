
lyahcms = 'http://lyahcms.appspot.com';


jQuery.prototype.cssShadow = function (s) {
    this.css('-moz-box-shadow', s)
        .css('-webkit-box-shadow', s)
        .css('box-shadow', s);
    return this; 
};

jQuery.prototype.cssBorderRadius = function (s) {
    this.css('-moz-border-radius', s)
        .css('border-radius', s);
    return this; 
};




var CommentsSystem = function () {
    this.ccounts = {};
    this.isCCInitialized = false;
    this.selectedPara = null;
    
    this.setup();
};


CommentsSystem.prototype.group = function () {
    return (/^.*\/(.*)\.html$/i).exec(location.pathname)[1];
} ();

CommentsSystem.prototype.select = function (para) {
    if (this.selectedPara)
        this.selectedPara.onUnselect();
    this.selectedPara = para;
    para.onSelect();
};

CommentsSystem.prototype.unselect = function () {
    if (this.selectedPara)
        this.selectedPara.onUnselect();
    this.selectedPara = null;
};

CommentsSystem.prototype.setup = function () {
    var self = this;
    $('img').css('position', 'relative').css('z-index', '69');
    
    $('.body p').not('.admonition-title').each(function (i) {
        new Paragraph(self, i, this);
    });
};

CommentsSystem.prototype.postComment = function (thread, c, cb)
{
    var self = this;
    var out = { 'group'  : this.group
              , 'thread' : thread
              , 'author' : c.author
              , 'url'    : c.url
              , 'text'   : c.text
              };
              
    $.getJSON(lyahcms + '/post?callback=?', out, function(data) {
        if (data.success)
            self.updateCommentsCount();
        cb(data.success);
    });
};

CommentsSystem.prototype.updateCommentsCount = function (cb) {
    var self = this;
    $.getJSON(lyahcms + '/count?callback=?', { 'group' : this.group},
        function (data) {
            if (data.success)
                self.ccounts = data.count;
            else
                self.ccounts = {};
            if (cb) cb();
        });
};
    
CommentsSystem.prototype.withCommentCount = function (thread, cb) {
    var self = this;
    if (!this.isCCInitialized) {
        this.updateCommentsCount(function () {
            self.isCCInitialized = true;
            cb(self.ccounts[thread] || 0);
        });
    }
    else 
        cb(this.ccounts[thread] || 0);
};

CommentsSystem.prototype.withComments = function (thread, cb) {
    $.getJSON(lyahcms + '/comments?callback=?',
        { 'group' : this.group, 'thread' : thread },
        function (data) {
            if(data.success) {
                var cs = [];
                $.each(data.cs, function (i, c) { cs.push(new Comment(c)); });
                cb(cs);
            }
        });
};





var Comment = function (c) {
    this.author = unescape(c.author);
    this.url = c.url;
    this.date = decodeURI(c.date);
    this.text = unescape(c.text);
};

Comment.prototype.attach = function (wrapper) {
    var title = this.url ? '<a href="' + this.url + '">' + this.author + '</a>'
                         : this.author;
    var str = '<div class="comment"><h5>' + title + '</h5><h6>' + this.date
            + '</h6><pre>' + this.text + '</pre></div>';    
    wrapper.append(str);
};



var CommentsBox = function (cs, p) {
    this.comments = [];
    this.cs = cs;
    this.para = p;
    this.wrapper = null;
    this.form = new Form(cs, this);
    
    this.attach();
};
                
CommentsBox.prototype.attach = function () {
    this.para.contents.append('<div class="comments"></div>');
    this.wrapper = this.para.contents.find('.comments');
    this.wrapper.css('padding', '3ex 6ex 1ex 6ex');
    this.wrapper.hide();
};

CommentsBox.prototype.loadComments = function (cb) {
    var self = this;
    this.cs.withComments(this.para.thread, function (comments) {
        self.comments = comments;
        $.each(comments, function (i, c) { c.attach(self.wrapper); });
        self.setupStyle();
        if (cb) cb();
    });
};

CommentsBox.prototype.onSubmit = function () {
    var self = this;
    this.cs.postComment(this.para.thread, this.form.getComment(), function (){
       self.cs.unselect();
    });
};

CommentsBox.prototype.setupStyle = function() {
    this.wrapper.find('.comment').css('background-color', '#ffe1fa')
                                 .cssBorderRadius('10px')
                                 .cssShadow('0 0 3px 3px #ffe1fa')
                                 .css('margin-top', '2ex');
    this.wrapper.find('h5').css('color', '#555')      
                           .css('margin-bottom', '0');
    this.wrapper.find('a').css('color', '#555');
    this.wrapper.find('h6').css('font-size', 'x-small')
                           .css('color', '#555')
                           .css('line-height', '0');
    this.wrapper.find('pre').css('text-indent', 0)
                            .css('font-size', 'small')
                            .css('font-family', 'Consolas, "Courier New", monospace')
                            .css('color', '#5A5A5A')
                            .css('line-height', '2ex')
                            .css('padding', '2ex 0 0 6ex')
                            .css('white-space', 'pre-wrap')
                            .css('white-space', '-moz-pre-wrap')
                            .css('white-space', '-pre-wrap')
                            .css('white-space', '-o-pre-wrap');
};

CommentsBox.prototype.show = function () {
    var self = this;
    this.loadComments(function () {
        self.form.show();
        self.wrapper.slideDown('fast');        
    });
};

CommentsBox.prototype.hide = function () {
    var self = this;
    this.wrapper.slideUp('fast', function () {
        self.form.hide();        
        self.wrapper.empty();
    });    
};





var Form = function (cs, cb) {
    this.cs = cs;
    this.csBox = cb;
    this.form = null;
    this.isSubmited = false;
};

Form.prototype.src = '<div class="cmtform"><h4>¡Comentar es gratis!</h4><form><p><input type="text" name="author_name" id="author_name" size="40" tabindex="1"><label id="anlabel" for="author_name">Nombre (obligatorio)</label></p><p><input type="text" name="author_url" id="author_url" size="40" tabindex="2"><label id="aulabel" for="author_url">Página web (opcional, incluir protocolo (\"http:\\\\\", \"https:\\\\\"))</label><p/><p><textarea name="text" id="cmttext" rows="7" cols="60" tabindex="3"></textarea><p/><p><input type="submit"id="submit" tabindex="4" value="Enviar"></p></form></div>';

Form.prototype.getComment = function ()
{
    return new Comment({ author : escape(this.form.find('#author_name').val())
                       , url    : encodeURI(this.form.find('#author_url').val())
                       , text   : escape(this.form.find('#cmttext').val())
                       });
};    

Form.prototype.hide = function ()
{
    this.form.remove();
    this.form = null;
};

Form.prototype.setup = function ()
{   
    var self = this;
    var submit = this.form.find('#submit');
    this.isSubmited = false;
    this.form.css('display', 'block')
             .css('border-top', 'solid 1px #AAA')
             .css('margin-top', '2ex');
    this.form.find('h4').css('color', '#555')
                        .css('text-indent', '0');
    this.form.find('p').css('text-indent', '0')
                       .css('font-size', 'small')    
                       .css('color', '#555')
                       .css('padding', '0')
                       .css('margin', '0');
    this.form.find('label').css('padding-left', '1ex');
    submit.css('margin-top', '1ex');
    submit.click(function (e) {
        e.preventDefault();
        if(!self.isSubmited) {
            self.isSubmited = true;
            self.csBox.onSubmit();
        }
    });
};

Form.prototype.show = function ()
{
    this.csBox.wrapper.append(this.src);
    this.form = this.csBox.wrapper.find('.cmtform');
    this.setup();
};



var Paragraph = function (cs, thread, contents) {
    var self = this;
    this.contents = $(contents);
    this.cs = cs;
    this.isHover = false;
    this.thread = thread;
    this.toggle = new Toggle(cs, this);
    this.csBox = new CommentsBox(cs, this);
    
    this.attach();
    this.setDefaultStyle();
    
    cs.withCommentCount(thread, function (c) {
        if (c !== 0) {
            self.contents.append('<div class="ccount"><p>' + c + '</p></div>');
            self.setCountStyle();
        }
    });
};


Paragraph.prototype.attach = function () {
    var self = this;
    this.contents.wrap('<div class="par-wrapper">');

    this.contents = this.contents.parent();
    
    this.contents.hover( function () { self.onHoverIn();  }
                       , function () { self.onHoverOut(); }
                       );
};

Paragraph.prototype.isSelected = function() {
    var thread = this.cs.selectedPara ? this.cs.selectedPara.thread : -1;
    return this.thread === thread;
};

Paragraph.prototype.onHoverIn = function () {
    if(!this.isSelected()) {    
        this.isHover = true;
        this.setBackground('#ffe1fa');
        this.toggle.show();
    }
};

Paragraph.prototype.onHoverOut = function () {
    if(!this.isSelected()) {
        this.isHover = false;
        this.setBackground('transparent');
        this.toggle.hide();
    }
};

Paragraph.prototype.onSelect = function () {
    this.setBackground('#fff1fd');
    this.toggle.hide();
    this.csBox.show();
};

Paragraph.prototype.onUnselect = function () {
    this.setBackground('transparent');
    this.csBox.hide();
};

Paragraph.prototype.setBackground = function (color) {
    this.contents.css('background-color', color)
                 .cssShadow('0 0 5px 5px ' + color);
};

Paragraph.prototype.setDefaultStyle = function () {
     this.contents.css('position', 'relative')
                  .cssBorderRadius('10px');
};

Paragraph.prototype.setCountStyle = function () {
    $(this.contents.find('.ccount')[0]).css('height', '42px')
                                       .css('width', '43px')
                                       .css('background-image', 'url("../_static/count.png")')
                                       .css('position', 'absolute')
                                       .css('top', '0')
                                       .css('left', '-40px');
    $(this.contents.find('.ccount p')[0]).css('font-size', '10px')
                                         .css('margin', '11px 0 0 7px')
                                         .css('width', '25px');
};




var Toggle = function (cs, p) {
    this.cs = cs;
    this.link = null;
    this.para = p;
    this.wrapper = null;
    this.isAttached = false;
    this.isClicked = false;
};


Toggle.prototype.setLabel = function () {
    var self = this;
    this.cs.withCommentCount(this.para.thread, function(c) {
        var str = (c === 0) ? 'Sin comentarios' :
                  (c === 1) ? '1 Comentario'    : c + ' Comentarios';
        self.link.text(str);
    });
};

Toggle.prototype.setup = function () {
    var self = this;
    var src = '<div class="toggle"><a>Cargando...</a></div>';
    this.para.contents.prepend(src);
    this.wrapper = this.para.contents.find('.toggle');
    this.link = this.para.contents.find('.toggle a');
    this.setupStyle();

    this.link.click(function (e) {
        e.preventDefault();
        if (!self.isClicked) {
            self.isClicked = true;
            self.cs.select(self.para);
            self.link.text('Cargando...');
        }
    });
};

Toggle.prototype.setupStyle = function () {
    var x = this.para.contents.width()  / 2 - this.link.width() / 2;
    var y = this.para.contents.height() / 2 - this.wrapper.height();
        
    this.wrapper.css('position', 'absolute')
                .css('display', 'none')
                .css('background-color', 'rgba(230,230,230,0.9)')
                .css('border', 'solid 1px rgba(40,40,40,0.6)')
                .css('border-bottom', 'solid 1px rgba(40,40,40,0.75)')
                .css('border-top', 'solid 1px rgba(40,40,40,0.3)')
                .css('cursor', 'pointer')
                .css('padding', '0 5px 0 5px')
                .cssShadow('0 0 2px 2px rgba(60,60,60,0.25)')
                .cssBorderRadius('10px')
                .css('top', y + 'px')
                .css('left', x + 'px'); 
    this.link.css('color', '#222')
             .css('text-decoration', 'none')
             .css('text-shadow','0px 1px 1px rgba(255,255,255,1)');
};

Toggle.prototype.show = function () {
    var self = this;
    if (!this.isAttached) {
        this.setup();
        this.isAttached = true;
    }
    this.setLabel();
    this.wrapper.fadeIn('slow', function () {
        if(!self.para.isHover)
            self.hide();
    });
};

Toggle.prototype.hide = function () {
    this.wrapper.hide();
    this.isClicked = false;
};




$(function () {
    CommentsSystem.prototype.instance = new CommentsSystem();
});

lyahcms = 'http://lyahcms.appspot.com'


function postComment (thread, name, url, text) {
    $.getJSON(lyahcms + '/post?callback=?',
              { 'name'     : name
              , 'url'      : url
              , 'thread'   : thread
              , 'text'     : text
              });   
}

function getComments (thread, cb) {
    $.getJSON(lyahcms + '/comments?callback=?',
              { 'thread' : thread  }, cb);
}

function getCommentsCount (thread, cb) {
    $.getJSON(lyahcms + '/count?callback=?', { 'thread' : thread},
        function (data) {
            var count = data.count || 0;
            cb(count);
        });
}


function getThreadId () {
    if (!getThreadId.id)
        getThreadId.id = /^.*\/(.*)\.html$/i.exec(location.pathname)[1];
    return getThreadId.id;
}

/*
 * Utility
 */

function setCSSBoxShadow (o, s) {
    $(o).css('-moz-box-shadow', s)
        .css('-webkit-box-shadow', s)
        .css('box-shadow', s);
}

function setCSSBorderRadius (o, s) {
    $(o).css('-moz-border-radius', s)
        .css('border-radius', s);
}


/*
 * Comment weappers
 */
 
function makeCmtWrapper (i, obj) {
    $(obj).wrap('<div class= "cmt-wrapper" id ="' + getThreadId() + i + '">');
}

function selectCmtWrapper (obj) {
    deselectCmtWrapper();

    $(obj).addClass('selected');
    $(obj).css('background-color', '#dfdfdf');
    setCSSBoxShadow(obj, '0 0 5px 5px #dfdfdf');
          
}

function deselectCmtWrapper () {
    $('.selected').each(function (i) {
        cs = $(this).children('.comments');
        cs.slideUp('fast', function () {
          cs.remove();  
        });
        $(this).removeClass('selected');
        setCmtWrapperDefaultStyle(this);        
    });
}

function isCmtWrapperSelected (obj) {
    return $(obj).hasClass('selected');
}


function setCmtWrapperDefaultStyle (obj) {
    $(obj).css('background-color', 'transparent');
    setCSSBoxShadow(obj, '0 0 0 0 transparent');
}

function setCmtWrapperHoverStyle (obj) {
    $(obj).css('position', 'relative').css('background-color', '#f5d96e');
    setCSSBorderRadius(obj, '7px');
    setCSSBoxShadow(obj, '0 0 5px 5px #f5d96e');
}

function addCmtWrapperLabel (obj) {
    removeCmtWrapperLabel();
    if(isCmtWrapperSelected(obj)) return;
    $(obj).prepend('<div id="cmt-label"><a>Comentarios</a></div>');
    
    var id = $(obj).attr('id');
    var label = $('#cmt-label');
    var link = $('#cmt-label a');
        
    label.css('position', 'absolute')
         .css('display', 'none')
         .css('top', ($(obj).height() / 2 - label.height() / 2) + 'px')
         .css('left', ($(obj).width() / 2 - label.width() / 2) + 'px')
         .css('background-color', 'rgba(230,230,230,0.9)')
         .css('border', 'solid 1px rgba(40,40,40,0.6)')
         .css('border-bottom', 'solid 1px rgba(40,40,40,0.75)')
         .css('border-top', 'solid 1px rgba(40,40,40,0.3)')
         .css('cursor', 'pointer')
         .css('padding', '0 5px 0 5px');
    setCSSBoxShadow(label, '0 0 2px 2px rgba(60,60,60,0.25)');
    setCSSBorderRadius(label, '10px');
    link.css('color', '#222')
        .css('text-decoration', 'none')
        .css('text-shadow','0px 1px 1px rgba(255,255,255,1)');
           
    label.fadeIn('slow');
    
    getCommentsCount(id, function (c) {
        var str = (c == 0) ? 'Sin comentarios' :
                  ((c == 1) ? '1 Comentario' : c + ' Comentarios');
        link.text(str);
    });
    
    link.click(function (e) {
        e.preventDefault();
        selectCmtWrapper(obj);
        link.text('Cargando...');
        getComments(id, addCmtList);
    });
}

function showComment(cmt) {
    title = cmt.url ? '<a href="' + cmt.url + '">' + cmt.name + '</a>'
                    : cmt.name;
    return '<div class="comment"><h5>' + title + '</h5><h6>' + cmt.date
           + '</h6><p>' + cmt.text + '</p></div>';
}

function addCmtList(data) {
    $('#cmt-label').fadeOut();
    var wrapper = $('.selected');
    
    var cmts = '<div class="comments">';
    for (c in data.cs) {
        cmts += showComment(data.cs[c]);   
    }
    cmts += '</div>';
    
    wrapper.append(cmts);
    addCmtForm();
    var cmtslist = wrapper.children('.comments');
    var wpad = wrapper.width() / 16;
    cmtslist.hide().slideDown('fast');

    cmtslist.css('margin', '0 ' + wpad + 'px 0 ' + wpad + 'px')
            .css('position', 'static');
    $('.comments h5').css('color', '#555')
                     .css('margin-bottom', '0');
    $('.comments a').css('color', '#555');
    $('.comments h6').css('font-size', 'x-small')
                     .css('color', '#555')
                     .css('line-height', '0');
    $('.comments p').css('text-indent', 0)
                    .css('font-size', 'small')
                    .css('color', '#5A5A5A')
                    .css('padding-top', '1ex')
                    .css('padding-left', wpad / 2 + 'px');
}

function addCmtForm() {
    var wrapper = $('.selected');
    var cmtslist = wrapper.children('.comments');
    var form = '<div class="cmtform"><h4>¡Comentar es gratis!</h4><form><p><input type="text" name="author_name" id="author_name" size="40" tabindex="1"><label id="anlabel" for="author_name">Nombre (obligatorio)</label></br><input type="text" name="author_url" id="author_url" size="40" tabindex="2"><label id="aulabel" for="author_url">Página web (opcional)</label><br/><textarea name="text" id="cmttext" rows="3" cols="60" tabindex="3"></textarea><br/><input type="submit"id="submit" tabindex="4" value="Enviar"></p></form></div>'

    cmtslist.append(form);

    var cmtform = $('.cmtform');
    cmtform.css('display', 'block')
           .css('border-top', 'solid 1px #AAA')
           .css('padding-top', '16px')
           .css('line-height', '0');
    $('h4').css('color', '#555');
    $('#submit').click(function (e) {
        e.preventDefault();
        postComment($('.selected').attr('id'), $('#author_name').val(),
                    $('#author_url').val(), $('#cmttext').val());
        deselectCmtWrapper();
    });

}

function removeCmtWrapperLabel () {
    if($('#cmt-label').length)
        $('#cmt-label').remove();
}

function setCmtWrappersPrts () {
    $('.cmt-wrapper').hover(
        function () {
            if(!isCmtWrapperSelected(this)) {            
                setCmtWrapperHoverStyle(this);
                addCmtWrapperLabel(this);
            }
        },
        function () {
            if(!isCmtWrapperSelected(this)) {
                setCmtWrapperDefaultStyle(this);
                removeCmtWrapperLabel();
            }
        });
}

$(window).load(function () {
    console.log("starting...");
    console.log(getThreadId());
    $('img').css('position', 'relative').css('z-index', '69');
    $('.body p').not('.first').each(function (i) {
        makeCmtWrapper(i, this);
    });
    setCmtWrappersPrts();
});

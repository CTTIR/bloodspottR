/* Read-only slide inspection. OpenSeadragon is distributed under BSD-3-Clause. */
(function () {
  'use strict';
  let viewer, svg, payload, visible = true, selected = '';
  let trackers = [];
  const ns = 'http://www.w3.org/2000/svg';
  function annotationPath(g) {
    const line = a => a.map((p, i) => (i ? 'L' : 'M') + p[0] + ',' + p[1]).join(' ');
    if (g.type === 'LineString') return line(g.coordinates);
    if (g.type === 'MultiLineString') return g.coordinates.map(line).join(' ');
    if (g.type === 'Polygon') return g.coordinates.map(a => line(a) + ' Z').join(' ');
    if (g.type === 'MultiPolygon') return g.coordinates.flatMap(p => p.map(a => line(a) + ' Z')).join(' ');
    return '';
  }
  function selectAnnotation(id, notify) {
    selected = id;
    if (svg) svg.querySelectorAll('[data-annotation]').forEach(el => {
      const active = el.dataset.annotation === id;
      el.setAttribute('stroke', active ? '#ffcb45' : '#00d4dc');
      el.setAttribute('stroke-width', active ? '4' : '2');
    });
    if (notify && payload) Shiny.setInputValue('viewer_annotation', {image_id:payload.image_id,id:id}, {priority:'event'});
  }
  function adjustPoints() {
    if (!svg || !viewer.world.getItemCount()) return;
    const size = viewer.world.getItemAt(0).getContentSize();
    const radius = 5 * size.x / Math.max(1, viewer.container.clientWidth) / viewer.viewport.getZoom(true);
    svg.querySelectorAll('circle').forEach(el => el.setAttribute('r', radius));
  }
  function overlays() {
    trackers.forEach(t => t.destroy()); trackers = [];
    viewer.clearOverlays(); svg = null;
    if (!payload || payload.empty || !viewer.world.getItemCount()) return;
    const size = viewer.world.getItemAt(0).getContentSize();
    svg = document.createElementNS(ns, 'svg');
    svg.setAttribute('viewBox', `0 0 ${size.x} ${size.y}`);
    svg.setAttribute('preserveAspectRatio', 'none');
    svg.style.pointerEvents = 'none'; svg.style.opacity = visible ? '1' : '0';
    (payload.features || []).forEach(f => {
      const points = f.geometry.type === 'Point' ? [f.geometry.coordinates] : f.geometry.type === 'MultiPoint' ? f.geometry.coordinates : null;
      const elements = points ? points.map(p => {
        const e = document.createElementNS(ns,'circle'); e.setAttribute('cx',p[0]); e.setAttribute('cy',p[1]); return e;
      }) : [document.createElementNS(ns,'path')];
      elements.forEach(e => {
        if (!points) e.setAttribute('d',annotationPath(f.geometry));
        e.dataset.annotation = f.id; e.setAttribute('fill', '#00d4dc22'); e.setAttribute('fill-rule','evenodd');
        e.setAttribute('stroke','#00d4dc'); e.setAttribute('stroke-width','2'); e.setAttribute('vector-effect','non-scaling-stroke');
        e.style.pointerEvents = visible ? 'auto' : 'none'; e.style.cursor = 'pointer';
        const title = document.createElementNS(ns,'title'); title.textContent = f.id; e.appendChild(title);
        trackers.push(new OpenSeadragon.MouseTracker({element:e, clickHandler:ev => {
          ev.preventDefaultAction = true; selectAnnotation(f.id,true);
        }}));
        svg.appendChild(e);
      });
    });
    viewer.addOverlay({element:svg,location:new OpenSeadragon.Rect(0,0,1,size.y/size.x),checkResize:false});
    adjustPoints(); selectAnnotation(selected,false);
  }
  function init() {
    if (!document.getElementById('slide-viewer') || !window.Shiny || !window.OpenSeadragon) return;
    const mobile = window.matchMedia('(max-width: 767px)');
    const filters = document.querySelector('.bs-filters');
    const fitFilters = () => { if (filters) filters.open = !mobile.matches; };
    fitFilters(); mobile.addEventListener('change',fitFilters);
    viewer = OpenSeadragon({id:'slide-viewer',showNavigationControl:false,showNavigator:false,
      maxZoomPixelRatio:4,visibilityRatio:0.5,constrainDuringPan:true,blendTime:0.1,
      gestureSettingsMouse:{clickToZoom:false,dblClickToZoom:true},
      gestureSettingsTouch:{pinchToZoom:true},imageLoaderLimit:2});
    viewer.addHandler('open',overlays); viewer.addHandler('animation',adjustPoints);
    viewer.addHandler('open-failed',()=>{
      Shiny.setInputValue('viewer_load_error','The selected image could not be displayed.',{priority:'event'});
    });
    document.getElementById('viewer-home').onclick = () => viewer.viewport.goHome();
    document.getElementById('viewer-in').onclick = () => viewer.viewport.zoomBy(1.5);
    document.getElementById('viewer-out').onclick = () => viewer.viewport.zoomBy(1/1.5);
    Shiny.addCustomMessageHandler('bloodspottr-image', next => {
      const same = payload && payload.url === next.url && !next.empty;
      payload = next;
      Shiny.setInputValue("viewer_load_error",null);
      if (next.empty) { trackers.forEach(t=>t.destroy()); trackers=[]; viewer.close(); svg = null; return; }
      if (same) { overlays(); return; }
      selected = '';
      if (next.type === 'image') viewer.open({type:'image',url:next.url});
      else viewer.open({width:next.width,height:next.height,tileSize:254,tileOverlap:1,minLevel:0,maxLevel:next.max_level,
        getTileUrl:(level,x,y)=>next.url + (next.url.includes('?')?'&':'?') + `level=${level}&x=${x}&y=${y}`});
    });
    Shiny.addCustomMessageHandler('bloodspottr-select', x => selectAnnotation(x.id,false));
    Shiny.addCustomMessageHandler('bloodspottr-annotations', x => {
      visible = x;
      if (svg) { svg.style.opacity = x?'1':'0'; svg.querySelectorAll('[data-annotation]').forEach(e=>e.style.pointerEvents=x?'auto':'none'); }
    });
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded',init); else init();
})();

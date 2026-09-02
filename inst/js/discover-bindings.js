// shiny.cov UI discovery script.
//
// Runs inside a real, live browser page (via shinytest2's AppDriver$get_js()
// or Cypress's cy.window()) to discover which inputs/outputs actually exist,
// by asking Shiny's own Shiny.inputBindings/Shiny.outputBindings registry --
// not by guessing from CSS classes. Every working Shiny input/output, from
// any widget library (base Shiny, shinyWidgets, shiny.fluent, htmlwidgets,
// or a fully custom Shiny.inputBindings.register() call), must register a
// binding there for Shiny's client-server protocol to work at all, so this
// generalizes to any framework without shiny.cov needing to know it exists.
//
// Tabs/conditional panels are a partial exception: Shiny has no equivalent
// registry for navigation/visibility constructs (they aren't inputs or
// outputs), so that part still relies on Shiny's own Bootstrap-based markup
// conventions -- less framework-agnostic than the input/output discovery
// above, but there's no Shiny-native registry to query instead, and it still
// reflects the actually-rendered DOM rather than a static server-side guess.
//
// shiny.cov-cypress vendors this exact file (see
// shiny.cov-cypress/scripts/sync-discover-bindings.js and
// shiny.cov-cypress/src/vendor/discover-bindings-source.js) so the Cypress
// adapter runs the identical script rather than a separately maintained
// copy. Re-run that sync script after editing this file.
(function () {
  function dedupeById(entries) {
    // A single element can match more than one registered binding (e.g. a
    // plain <input> can match both a generic base binding and a more
    // specific custom one) -- prefer the more specific, non-"shiny."
    // prefixed binding name when that happens.
    var byId = {};
    entries.forEach(function (e) {
      var existing = byId[e.id];
      if (!existing) {
        byId[e.id] = e;
        return;
      }
      var existingIsBase = existing.type.indexOf("shiny.") === 0;
      var currentIsBase = e.type.indexOf("shiny.") === 0;
      if (existingIsBase && !currentIsBase) {
        byId[e.id] = e;
      }
    });
    return Object.keys(byId).map(function (k) {
      return byId[k];
    });
  }

  function labelFor(id) {
    try {
      var el = document.querySelector('label[for="' + CSS.escape(id) + '"]');
      return el ? el.textContent.trim() : "";
    } catch (e) {
      return "";
    }
  }

  function discover(registry) {
    var results = [];
    if (!registry || !registry.bindingNames) return results;
    // Shiny bindings are written against a jQuery scope: shiny.react,
    // htmlwidgets' plotly/datatables, etc. call scope.find() / $(scope).find()
    // and throw on a raw DOM document. Wrap it in jQuery when present (every
    // Shiny app ships jQuery) so those bindings are discoverable.
    var scope = (typeof jQuery !== "undefined" && jQuery.fn && jQuery.fn.jquery)
      ? jQuery(document)
      : document;
    var names = registry.bindingNames;
    for (var name in names) {
      if (!Object.prototype.hasOwnProperty.call(names, name)) continue;
      var binding = names[name].binding;
      var els;
      try {
        els = binding.find(scope);
      } catch (e) {
        continue;
      }
      for (var i = 0; i < els.length; i++) {
        var el = els[i];
        var id;
        try {
          id = binding.getId(el);
        } catch (e) {
          id = el.id;
        }
        if (!id) continue;
        results.push({ id: id, type: name });
      }
    }
    return dedupeById(results);
  }

  var inputs = discover(window.Shiny && window.Shiny.inputBindings).map(function (e) {
    return { id: e.id, type: e.type, label: labelFor(e.id) };
  });
  var outputs = discover(window.Shiny && window.Shiny.outputBindings).map(function (e) {
    return { id: e.id, type: e.type };
  });

  var tabs = [];
  document.querySelectorAll(".tab-pane[title]").forEach(function (el) {
    var title = el.getAttribute("title");
    if (title && tabs.indexOf(title) === -1) tabs.push(title);
  });
  document.querySelectorAll("a[data-toggle]").forEach(function (el) {
    var title = (el.textContent || "").trim();
    if (title && tabs.indexOf(title) === -1) tabs.push(title);
  });

  var conditional = [];
  document.querySelectorAll(".shiny-panel-conditional[id]").forEach(function (el) {
    if (conditional.indexOf(el.id) === -1) conditional.push(el.id);
  });

  return {
    inputs: inputs,
    outputs: outputs,
    tabs: tabs,
    conditional: conditional,
    modules: [] // populated separately by the shiny::moduleServer() hook
  };
})();

export function RegisterClasses() {
    var jsdir = "./netvcl/js/",
        cssdir = "./netvcl/css/",
        bsjs = jsdir + "bs/bootstrap.min.js",
        bscss = cssdir + "bs/bootstrap.min.css";


    //Base
    TApplication.RegisterClass('TNVPage', "./nv.forms.js");
    TApplication.RegisterClass('TNVFrame', "./nv.forms.js");
    TApplication.RegisterClass('TImage', "./nv.extctrls.js");
    TApplication.RegisterClass('TNvMessageDialog', "./nv.dialogs.js");
    TApplication.RegisterClass('TNvButtonsDialog', "./nv.dialogs.js");


    TApplication.RegisterClass('TTable', "./nv.table.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TNvChart', "./nv.charts.js")
        .RegisterClassModule(jsdir + 'Chart.bundle.min.js', 'js')
        .RegisterClassModule(cssdir + 'Chart.min.css', 'css');
    TApplication.RegisterClass('TNvStatusBar', "./nv.stdctrls.js");






    //Report
    TApplication.RegisterClass('TNvReport', "./report/nv.report.js")
        .RegisterClassModule(cssdir + 'report/nv.report.css', 'css');
    TApplication.RegisterClass('TNvrPanel', "./report/nv.report.js")
        .RegisterClassModule(cssdir + 'report/nv.report.css', 'css');
    TApplication.RegisterClass('TNvrText', "./report/nv.report.js")
        .RegisterClassModule(cssdir + 'report/nv.report.css', 'css');
    TApplication.RegisterClass('TNvrTable', "./report/nv.report.js")
        .RegisterClassModule(cssdir + 'report/nv.report.css', 'css')
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');

    //Bootstrap
    TApplication.RegisterClass('TBsPage', "./bs/nv.bs.forms.js");
    TApplication.RegisterClass('TNvBSText', "./bs/nv.bs.htmlcontrols.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TNvBsAlert', "./bs/nv.bs.alerts.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css')
        .RegisterClassModule(cssdir + "bs/nv.bs.css", 'css');
    TApplication.RegisterClass('TNvBsBadge', "./bs/nv.bs.alerts.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css')
        .RegisterClassModule(cssdir + "bs/nv.bs.css", 'css');
    TApplication.RegisterClass('TNvBsBadgeLink', "./bs/nv.bs.alerts.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css')
        .RegisterClassModule(cssdir + "bs/nv.bs.css", 'css');
    TApplication.RegisterClass('TNvBsToast', "./bs/nv.bs.alerts.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css')
        .RegisterClassModule(cssdir + "bs/nv.bs.css", 'css');
    TApplication.RegisterClass('TNvBsSpinner', "./bs/nv.bs.alerts.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css')
        .RegisterClassModule(cssdir + "bs/nv.bs.css", 'css');

    TApplication.RegisterClass('TNvBsButton', "./bs/nv.bs.buttons.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css')
        .RegisterClassModule(cssdir + "bs/nv.bs.css", 'css');
    TApplication.RegisterClass('TNvBsButtonGroup', "./bs/nv.bs.buttons.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TNvBsButtonToolbar', "./bs/nv.bs.buttons.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TButtonDropdown', "./bs/nv.bs.buttons.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TNvBsDropDown', "./bs/nv.bs.buttons.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');


    TApplication.RegisterClass('TNvBsContainer', "./bs/nv.bs.containers.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TNvBsRow', "./bs/nv.bs.containers.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TNvBsFormRow', "./bs/nv.bs.containers.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TNvBsColumn', "./bs/nv.bs.containers.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TNvBsListItem', "./bs/nv.bs.containers.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');   
    TApplication.RegisterClass('TNvBsListGroup', "./bs/nv.bs.containers.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');

    TApplication.RegisterClass('TNvBsScrollFocus', "./bs/nv.bs.scrollfocus.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css')
        .RegisterClassModule(cssdir + "bs/nv.bs.css", 'css');



    TApplication.RegisterClass('TNvBsCard', "./bs/nv.bs.cards.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TNvBsCardHeader', "./bs/nv.bs.cards.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TNvBsCardBody', "./bs/nv.bs.cards.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TNvBsCardFooter', "./bs/nv.bs.cards.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TNvBsCardTitle', "./bs/nv.bs.cards.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TNvBsCardSubtitle', "./bs/nv.bs.cards.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TNvBsCardLink', "./bs/nv.bs.cards.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TNvBsCardText', "./bs/nv.bs.cards.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');

    TApplication.RegisterClass('TNvBsCardStats', "./bs/nv.bs.cards.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css')
        .RegisterClassModule(cssdir + "bs/nv.bs.css", 'css');
    TApplication.RegisterClass('TNvBsCardChart', "./bs/nv.bs.cards.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css')
        .RegisterClassModule(cssdir + "bs/nv.bs.css", 'css');
    TApplication.RegisterClass('TNvBsCardTable', "./bs/nv.bs.cards.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css')
        .RegisterClassModule(cssdir + "bs/nv.bs.css", 'css');




    TApplication.RegisterClass('TNvBsAccordion', "./bs/nv.bs.accordion.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TNvBsAccordionItem', "./bs/nv.bs.accordion.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TNvBsAccordionBody', "./bs/nv.bs.accordion.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');


    TApplication.RegisterClass('TNvBsNav', "./bs/nv.bs.navbar.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TNvBsNavLink', "./bs/nv.bs.navbar.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TBsNavItemLink', "./bs/nv.bs.navbar.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TBsNavItemDropdown', "./bs/nv.bs.navbar.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');


    TApplication.RegisterClass('TNvBsLink', "./bs/nv.bs.htmlcontrols.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TNvBsDropdownItemLink', "./bs/nv.bs.htmlcontrols.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TNvBsDropdownMenu', "./bs/nv.bs.htmlcontrols.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TNvBsNavBar', "./bs/nv.bs.navbar.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TBsNavBarContainer', "./bs/nv.bs.navbar.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TBsNavBarBrand', "./bs/nv.bs.navbar.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TBsNavBarToggler', "./bs/nv.bs.navbar.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TBsNavBarContent', "./bs/nv.bs.navbar.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TBsNavBarNav', "./bs/nv.bs.navbar.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TBsNavBarItemLink', "./bs/nv.bs.navbar.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TBsNavBarItemDropdown', "./bs/nv.bs.navbar.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TBsNavItemCollapse', "./bs/nv.bs.navbar.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TNvBsSideBar', "./bs/nv.bs.navbar.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css')
        .RegisterClassModule(cssdir + "bs/nv.bs.css", 'css');


    TApplication.RegisterClass('TNvBsTabControl', "./bs/nv.bs.tabs.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TNvBsTabs', "./bs/nv.bs.tabs.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TNvBsTabHeaderLink', "./bs/nv.bs.tabs.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TNvBsTabDropdown', "./bs/nv.bs.tabs.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TNvBsTabContent', "./bs/nv.bs.tabs.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');
    TApplication.RegisterClass('TNvBsTab', "./bs/nv.bs.tabs.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');



    TApplication.RegisterClass('TNvBsInput', "./bs/nv.bs.inputs.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css')
        .RegisterClassModule(cssdir + "bs/nv.bs.css", 'css');
    TApplication.RegisterClass('TNvBsInputDate', "./bs/nv.bs.inputs.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(jsdir + 'bs/litepicker.min.js', 'js')
        .RegisterClassModule(bscss, 'css')
        .RegisterClassModule(cssdir + "bs/nv.bs.css", 'css');
    TApplication.RegisterClass('TNvBsDateRange', "./bs/nv.bs.inputs.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(jsdir + 'bs/litepicker.min.js', 'js')
        .RegisterClassModule(jsdir + 'bs/litepicker.ranges.js', 'js')
        .RegisterClassModule(bscss, 'css')
        .RegisterClassModule(cssdir + "bs/nv.bs.css", 'css');
    TApplication.RegisterClass('TNvBsInputDateTime', "./bs/nv.bs.inputs.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(jsdir + 'bs/easepick.min.js', 'js')
        .RegisterClassModule(bscss, 'css')
        .RegisterClassModule(cssdir + "bs/nv.bs.css", 'css');
    TApplication.RegisterClass('TNvBsSelect', "./bs/nv.bs.inputs.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css')
        .RegisterClassModule(cssdir + "bs/nv.bs.css", 'css');
    TApplication.RegisterClass('TNvBsMemo', "./bs/nv.bs.inputs.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css')
        .RegisterClassModule(cssdir + "bs/nv.bs.css", 'css');
    TApplication.RegisterClass('TNvBsRange', "./bs/nv.bs.inputs.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css')
        .RegisterClassModule(cssdir + "bs/nv.bs.css", 'css');
    TApplication.RegisterClass('TNvBsCheckbox', "./bs/nv.bs.inputs.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css')
        .RegisterClassModule(cssdir + "bs/nv.bs.css", 'css');
    TApplication.RegisterClass('TNvBsSwitch', "./bs/nv.bs.inputs.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css')
        .RegisterClassModule(cssdir + "bs/nv.bs.css", 'css');
    TApplication.RegisterClass('TNvBsRadioGroup', "./bs/nv.bs.inputs.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css')
        .RegisterClassModule(cssdir + "bs/nv.bs.css", 'css');
    TApplication.RegisterClass('TNvBsLookupSelect', "./bs/nv.bs.inputs.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css')
        .RegisterClassModule(cssdir + "bs/nv.bs.css", 'css');

    TApplication.RegisterClass('TNvBsInputGroup', "./bs/nv.bs.inputs.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css')
        .RegisterClassModule(cssdir + "bs/nv.bs.css", 'css');
    TApplication.RegisterClass('TNvBsInputAddonIcon', "./bs/nv.bs.inputs.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css')
        .RegisterClassModule(cssdir + "bs/nv.bs.css", 'css');
    TApplication.RegisterClass('TNvBsInputAddonText', "./bs/nv.bs.inputs.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css')
        .RegisterClassModule(cssdir + "bs/nv.bs.css", 'css');
    TApplication.RegisterClass('TNvBsInputAddonAction', "./bs/nv.bs.inputs.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css')
        .RegisterClassModule(cssdir + "bs/nv.bs.css", 'css');








    TApplication.RegisterClass('TNvBsTable', "./bs/nv.bs.table.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css')
        .RegisterClassModule(jsdir + "bs/bootstrap-table.min.js", 'js')
        .RegisterClassModule(cssdir + "bs/bootstrap-table.min.css", 'css');


    TApplication.RegisterClass('TNvBsDbTable', "./bs/nv.bs.table.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css')
        .RegisterClassModule(jsdir + "bs/bootstrap-table.min.js", 'js')
        .RegisterClassModule(cssdir + "bs/bootstrap-table.min.css", 'css');

    TApplication.RegisterClass('TNvDashboard', "./nv.dashboards.js")
        .RegisterClassModule(jsdir + "gridstack-all.js", 'js')
        .RegisterClassModule(cssdir + "gridstack.min.css", 'css');

    TApplication.RegisterClass('TNvDashbdItem', "./nv.dashboards.js")
        .RegisterClassModule(jsdir + "gridstack-all.js", 'js')
        .RegisterClassModule(cssdir + "gridstack.min.css", 'css');

    TApplication.RegisterClass('TNvBsImage', "./bs/nv.bs.images.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');

    TApplication.RegisterClass('TNvBsSearch', "./firedac/nv.bs.search.js")
        .RegisterClassModule(bsjs, 'js')
        .RegisterClassModule(bscss, 'css');




}
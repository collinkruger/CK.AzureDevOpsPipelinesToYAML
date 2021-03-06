module canopy

open canopy.classic
open OpenQA.Selenium

module Selectors =
    let descendent (selector:string) (el:IWebElement) =
        el.FindElement(By.CssSelector selector)

    let descendents (selector:string) (el:IWebElement) =
        el.FindElements(By.CssSelector selector) |> List.ofSeq

    let nextSibling (el:IWebElement) =
        (browser :?> OpenQA.Selenium.IJavaScriptExecutor).ExecuteScript("return arguments[0].nextSibling;", el) :?> IWebElement

    let matches (selector:string) (el:IWebElement) =
        (browser :?> OpenQA.Selenium.IJavaScriptExecutor).ExecuteScript("return arguments[0].matches(arguments[1]);", el, selector) :?> bool

    let rec ancestor (selector:string) (el:IWebElement) =
        if isNull el then
            null
        elif el |> matches selector then
            el
        else
            parent el |> ancestor selector

    let attr name (el:IWebElement) =
        el.GetAttribute(name)

    let findByText (str:string) =
        browser.FindElement(By.XPath($"//*[text()=\"{str}\"]")) // TODO: Needs to handle escaping characters I think

    let findByTextOpt (str:string) =
        try
            findByText str |> Some
        with
            | :? NoSuchElementException -> None

    let descendentByText (str:string) (el:IWebElement) =
        el.FindElement(By.XPath($"//*[text()=\"{str}\"]")) // TODO: Needs to handle escaping characters I think

    let descendentByTextOpt (str:string) =
        try
            descendentByText str |> Some
        with
            | :? NoSuchElementException -> None


module Tabs = 
    let ids () =
        browser.WindowHandles
        |> List.ofSeq

    let currentId () =
        browser.CurrentWindowHandle

    let currentIndex () =
        let currentTabId = currentId ()
        ids() |> List.findIndex ((=) currentTabId)

    let switchToIndex i =
        switchToTab (i + 1)

    let switchToId id =
        let ids = ids ()
        let i = ids |> List.findIndex ((=) id)
        switchToIndex i

    let closeById id =
       ids()
       |> List.findIndex ((=) id)
       |> (+) 1
       |> closeTab

    let closeByIndex i =
        i + 1 |> closeTab

    let closeCurrent () =
        currentId () |> closeById
using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

using Matryx;
using System.Numerics;
using Vector3 = UnityEngine.Vector3;
using Vector2 = UnityEngine.Vector2;

public class MyAccountMenu : MenuStateReceiver
{
    internal class AccountMenuResponder : FlexMenu.FlexMenuResponder
    {
        public FlexMenu menu;
        MyAccountMenu accountMenu;
        internal AccountMenuResponder(MyAccountMenu accountMenu, FlexMenu menu)
        {
            this.menu = menu;
            this.accountMenu = accountMenu;
        }

        public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
        {
            HandleInput(sender.gameObject);
        }

        public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider) { }
    }

    private static void HandleInput(GameObject source)
    {
        if (source.name.Contains("0x"))
        {
            NetworkSettings.setActiveAccount(source.name);
            Instance.GetComponent<AnimationHandler>().CloseMenu();
            MyAccountButton.Instance.updateBlockie();
        }
    }

    [SerializeField]
    public TMPro.TextMeshPro accountsText;
    [SerializeField]
    public TMPro.TextMeshPro accountInfoText;
    public static MyAccountMenu Instance { get; private set; }
    AccountMenuResponder responder;
    MultiSelectFlexPanel accountsPanel;
    private Scroll scroll;
    const int maxTextLength = 400;
    JoyStickAggregator joyStickAggregator;
    FlexMenu flexMenu;

    public void Awake()
    {
        if (!initialized) { Initialize(); }
    }

    bool initialized = false;
    public void Initialize()
    {
        if (Instance == null) { Instance = this; }

        scroll = GetComponentInChildren<Scroll>(true);
        flexMenu = GetComponent<FlexMenu>();
        AccountMenuResponder responder = new AccountMenuResponder(this, flexMenu);
        flexMenu.RegisterResponder(responder);
        accountsPanel = GetComponentInChildren<MultiSelectFlexPanel>(true);
        accountsPanel.Initialize();
        joyStickAggregator = scroll.GetComponent<JoyStickAggregator>();

        initialized = true;

        Refresh();
    }

    /// <summary>
    /// Clears the list of tournaments.
    /// </summary>
    public void ClearAccounts()
    {
        scroll.clear();
    }

    public void Refresh()
    {
        if (!initialized) { Initialize(); }

        if (NetworkSettings.mnemonicWallet != null)
        {
            var accounts = NetworkSettings.mnemonicWallet.GetAddresses();
            for (int i = 0; i < accounts.Length; i++)
            {
                GameObject button = createButton(accounts[i]);
                button.SetActive(false);
                accountsPanel.AddAction(button.GetComponent<FlexButtonComponent>());
            }

            MatryxCortex.GetMTXBalance((bal) => { accountInfoText.text = "Current:\n" + NetworkSettings.currentAddress + "\n\n Balance: " + bal + "MTX"; });
        }
    }

    private GameObject createButton(string account)
    {
        GameObject button = Instantiate(Resources.Load("Account_Cell", typeof(GameObject))) as GameObject;
        button.transform.SetParent(accountsPanel.transform);
        button.transform.localScale = Vector3.one;

        button.name = account;

        MeshRenderer meshRenderer = button.transform.Find("Icon").GetComponent<MeshRenderer>();
        Texture2D blockieTex = Utils.Accounts.getBlockieTexture(account);
        Material blockieMaterial = new Material(meshRenderer.material);
        blockieMaterial.mainTexture = blockieTex;

        meshRenderer.material = blockieMaterial;

        button.transform.Find("Text").GetComponent<TMPro.TextMeshPro>().text = Utils.Accounts.ellipseAddress(button.name);

        scroll.addObject(button.transform);
        joyStickAggregator.AddForwarder(button.GetComponentInChildren<JoyStickForwarder>());

        return button;
    }
}

using Matryx;
using Nanome.Core;
using System.Collections;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityEngine.EventSystems;
using UnityEngine.UI;
using Valve.VR;

public class MatryxAccountMenu : MonoBehaviour {

    [SerializeField]
    GameObject AccountUnlockMenu;
    [SerializeField]
    Text[] AccountUnlockText;
    [SerializeField]
    InputField PrivateKeyField;
    [SerializeField]
    InputField PasswordField;
    [SerializeField]
    Toggle UsePassword;

    [SerializeField]
    GameObject AccountInfoMenu;
    [SerializeField]
    public Text[] AccountInfoText;

    [SerializeField]
    UnlockAccountButton unlockButton;

    public static MatryxAccountMenu Instance { get; private set; }
    public AccountMenuState State { get; private set; }

    public enum AccountMenuState
    {
        PrivateKeyInput,
        PrivateKeyWithPasswordInput,
        PasswordInput,
        ShowAccountInfo
    }

	public void Start () {
		if (Instance == null)
        {
            Instance = this;
            OVRManager.HMDMounted += ReenterVR;
        }

    }

    public void OnEnable()
    {
        var cypher = Config.getString("sKCypher", "");
        var usedPassword = Config.getBool("usedPassword", "false");

        if (cypher == "")
        {
            SetState(AccountMenuState.PrivateKeyWithPasswordInput);
        }
        else if (usedPassword)
        {
            SetState(AccountMenuState.PasswordInput);
        }
        else
        {
            TryUnlockAccount();
            Instance.gameObject.SetActive(false);
        }
    }

    public static void SetState(AccountMenuState state)
    {
        Instance.gameObject.SetActive(true);
        Instance.State = state;

        switch (state)
        {
            case AccountMenuState.PrivateKeyInput:
                Instance.PasswordField.transform.parent.gameObject.SetActive(false);
                Instance.UsePassword.transform.parent.gameObject.SetActive(true);
                Instance.UsePassword.isOn = false;
                break;
            case AccountMenuState.PrivateKeyWithPasswordInput:
                Instance.PasswordField.transform.parent.gameObject.SetActive(true);
                Instance.UsePassword.transform.parent.gameObject.SetActive(true);
                Instance.UsePassword.isOn = true;
                break;
            case AccountMenuState.PasswordInput:
                Instance.PrivateKeyField.transform.parent.gameObject.SetActive(false);
                Instance.PasswordField.transform.parent.gameObject.SetActive(true);
                Instance.UsePassword.gameObject.SetActive(false);
                Instance.AccountUnlockText[0].text = "Welcome back!";
                Instance.AccountUnlockText[1].text = "Your private key has been encrypted with AES and stored in AppData.\n To decrypt your private key, we need your password!";
                Instance.PasswordField.placeholder.GetComponent<Text>().text = "password123";
                break;
            case AccountMenuState.ShowAccountInfo:
                Instance.AccountInfoText[0].text = NetworkSettings.address;
                Instance.AccountInfoText[1].text = "1 MTX";
                Instance.StartCoroutine(Instance.waitAndThenReenterVR());
                break;
            default:
                break;
        }

        bool isAccountInfo = state == AccountMenuState.ShowAccountInfo;
        Instance.AccountUnlockMenu.SetActive(!isAccountInfo);
        Instance.AccountInfoMenu.SetActive(isAccountInfo);
    }

    public IEnumerator waitAndThenReenterVR()
    {
        yield return new WaitForSeconds(15);
        ReenterVR();
    }

    public void toggleUsePassword()
    {
        if (Instance.UsePassword.isOn)
        {
            SetState(AccountMenuState.PrivateKeyWithPasswordInput);
        }
        else
        {
            SetState(AccountMenuState.PrivateKeyInput);
        }
    }

    public void TakeInput()
    {
        NetworkSettings.declinedAccountUnlock = false;

        switch (Instance.State)
        {
            case AccountMenuState.PasswordInput:
                if (!TryUnlockAccount()) return;
                break;
            default:
                if (!TrySetPrivateKeyAndCypher()) return;
                break;
        }

        ClearInputs(true);
        SetState(AccountMenuState.ShowAccountInfo);
    }

    public void Cancel()
    {
        NetworkSettings.declinedAccountUnlock = true;
        TournamentsMenu.SetState(TournamentsMenu.TournamentMenuState.AccountUnlockRequired);
        Instance.gameObject.SetActive(false);
    }

    public void ClearInputs(bool includeText = false)
    {
        if (includeText)
        {
            Instance.PrivateKeyField.text = "";
            Instance.PasswordField.text = "";
        }

        Instance.PrivateKeyField.GetComponent<Image>().color = Color.white;
        Instance.PasswordField.GetComponent<Image>().color = Color.white;

        Instance.AccountUnlockText[2].gameObject.SetActive(false);
    }

    public enum InputType
    {
        Password,
        PrivateKey
    }
    public static bool ValidateInput(InputType type, string input)
    {
        if (type == InputType.Password)
        {
            return input != "";
        }
        else
        {
            var pkRegEx = @"^0x[0-9A-Fa-f]{64}$";
            if (!Regex.IsMatch(input, pkRegEx)) return false;

            try
            {
                Nethereum.Signer.EthECKey.GetPublicAddress(input);
            }
            catch (System.Exception e)
            {
                return false;
            }

            return true;
        }
    }

    public void ReenterVR()
    {
        if (Instance.State == AccountMenuState.ShowAccountInfo)
        {
            Instance.gameObject.SetActive(false);
        }
    }
    
    // returns success
    public static bool TrySetPrivateKeyAndCypher()
    {
        string privateKey = Instance.PrivateKeyField.text;
        string password = Instance.PasswordField.text;
        bool usedPassword = Instance.UsePassword.isOn;

        if (!ValidateInput(InputType.PrivateKey, privateKey))
        {
            Instance.PrivateKeyField.GetComponent<Image>().color = new Color(1f, 181f / 255f, 181f / 255f);
            Instance.AccountUnlockText[2].text = "Invalid private key";
            Instance.AccountUnlockText[2].gameObject.SetActive(true);
            return false;
        }

        if (usedPassword && !ValidateInput(InputType.Password, password))
        {
            Instance.PasswordField.GetComponent<Image>().color = new Color(1f, 181f / 255f, 181f / 255f);
            Instance.AccountUnlockText[2].text = "Invalid password";
            Instance.AccountUnlockText[2].gameObject.SetActive(true);
            return false;
        }

        string cypher = Crypto.Encrypt(privateKey, usedPassword ? password : "");

        NetworkSettings.privateKey = privateKey;
        NetworkSettings.address = Nethereum.Signer.EthECKey.GetPublicAddress(NetworkSettings.privateKey);

        Config.setString("sKCypher", cypher, true, "storage");
        Config.setString("address", NetworkSettings.address);
        Config.setBool("usedPassword", usedPassword, true, "storage");

        TournamentsMenu.SetState(TournamentsMenu.TournamentMenuState.DisplayingTournaments);

        return true;
    }

    // returns success
    public static bool TryUnlockAccount()
    {
        var usedPassword = Config.getBool("usedPassword", "false");
        var password = Instance.PasswordField.text;

        string cypher = Config.getString("sKCypher", "");
        string decrypted = Crypto.Decrypt(cypher, password);

        if (decrypted.Substring(0, 2) != "0x")
        {
            Instance.PasswordField.GetComponent<Image>().color = new Color(1f, 181f / 255f, 181f / 255f);
            Instance.AccountUnlockText[2].text = "Invalid password";
            Instance.AccountUnlockText[2].gameObject.SetActive(true);
            return false;
        }

        NetworkSettings.declinedAccountUnlock = false;
        NetworkSettings.privateKey = decrypted;
        NetworkSettings.address = Nethereum.Signer.EthECKey.GetPublicAddress(NetworkSettings.privateKey);

        TournamentsMenu.SetState(TournamentsMenu.TournamentMenuState.DisplayingTournaments);

        return true;
    }

}

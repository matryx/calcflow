using Matryx;
using Nanome.Core;
using System.Collections;
using System.Text;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityEngine.EventSystems;
using UnityEngine.UI;
using Valve.VR;

public class MatryxAccountMenu : MonoBehaviour {

    [SerializeField]
    Dropdown accountImportType;
    [SerializeField]
    GameObject AccountUnlockMenu;
    [SerializeField]
    Text[] AccountUnlockText;
    [SerializeField]
    InputField PrivateKeyInput;
    [SerializeField]
    Image KeyStoreDropbox;
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
        MnemonicInput = 0,
        MnemonicPasswordInput,
        PrivateKeyInput,
        PrivateKeyPasswordInput,
        KeyStoreInput,
        PasswordRequired,
        ShowAccountInfo
    }

    public void Start()
    {
        if (Instance == null)
        {
            Instance = this;
            OVRManager.HMDMounted += ReenterVR;
            accountImportType.onValueChanged.AddListener(delegate
            {
                if (accountImportType.value == 0)
                {
                    SetState(AccountMenuState.MnemonicPasswordInput);
                }
                else if (accountImportType.value == 1)
                {
                    SetState(AccountMenuState.PrivateKeyPasswordInput);
                }
                else
                {
                    SetState(AccountMenuState.KeyStoreInput);
                }
            });
        }
    }

    public void OnEnable()
    {
        if (Instance == null)
        {
            Instance = this;
            OVRManager.HMDMounted += ReenterVR;
        }

        // must be created on the main thread to get the right thread id.
        //hook = new UnityDragAndDropHook();
        //hook.InstallHook();
        //hook.OnDroppedFiles += OnFiles;

        var cypher = Config.getString("sKCypher", "");
        var usedPassword = Config.getBool("usedPassword", "false");

        if (cypher == "")
        {
            SetState(AccountMenuState.MnemonicPasswordInput);
            accountImportType.onValueChanged.AddListener(delegate
            {
                if (accountImportType.value == 0)
                {
                    SetState(AccountMenuState.MnemonicPasswordInput);
                }
                else if (accountImportType.value == 1)
                {
                    SetState(AccountMenuState.PrivateKeyPasswordInput);
                }
                else
                {
                    SetState(AccountMenuState.KeyStoreInput);
                }
            });
        }
        else if (usedPassword)
        {
            SetState(AccountMenuState.PasswordRequired);
        }
        else
        {
            TryUnlockAccount();
            Instance.gameObject.SetActive(false);
        }
    }

    //void OnDisable()
    //{
    //    //hook.UninstallHook();
    //}

    public void configurePrivateKeyField(string title, string defaultText, bool active, string validationType)
    {
        Instance.PrivateKeyInput.transform.parent.GetChild(0).GetComponent<Text>().text = title;
        Instance.PrivateKeyInput.placeholder.GetComponent<Text>().text = defaultText;
        Instance.PrivateKeyInput.transform.parent.gameObject.SetActive(active);
        Instance.PrivateKeyInput.GetComponent<InputValidator>().setValidation(validationType);
    }

    public void SetState(AccountMenuState state)
    {
        Instance.gameObject.SetActive(true);
        Instance.State = state;

        switch (state)
        {
            case AccountMenuState.PrivateKeyInput:
                Instance.PasswordField.transform.parent.gameObject.SetActive(false);
                Instance.UsePassword.transform.parent.gameObject.SetActive(true);
                Instance.UsePassword.isOn = false;
                Instance.UsePassword.gameObject.SetActive(true);
                configurePrivateKeyField("Private Key", "0x...", true, "private key");
                Instance.KeyStoreDropbox.gameObject.SetActive(false);
                break;
            case AccountMenuState.PrivateKeyPasswordInput:
                Instance.PasswordField.transform.parent.gameObject.SetActive(true);
                Instance.UsePassword.transform.parent.gameObject.SetActive(true);
                Instance.UsePassword.isOn = true;
                Instance.UsePassword.gameObject.SetActive(true);
                configurePrivateKeyField("Private Key", "0x...", true, "private key");
                Instance.KeyStoreDropbox.gameObject.SetActive(false);
                break;
            case AccountMenuState.MnemonicInput:
                Instance.PasswordField.transform.parent.gameObject.SetActive(false);
                Instance.UsePassword.transform.parent.gameObject.SetActive(true);
                Instance.UsePassword.isOn = false;
                Instance.UsePassword.gameObject.SetActive(true);
                configurePrivateKeyField("Mnemonic Phrase", "Your mnemonic seed", true, "mnemonic");
                Instance.KeyStoreDropbox.gameObject.SetActive(false);
                break;
            case AccountMenuState.MnemonicPasswordInput:
                Instance.PasswordField.transform.parent.gameObject.SetActive(true);
                Instance.UsePassword.transform.parent.gameObject.SetActive(true);
                Instance.UsePassword.isOn = true;
                Instance.UsePassword.gameObject.SetActive(true);
                configurePrivateKeyField("Mnemonic Phrase", "Your mnemonic seed", true, "mnemonic");
                Instance.KeyStoreDropbox.gameObject.SetActive(false);
                break;
            case AccountMenuState.KeyStoreInput:
                Instance.PasswordField.transform.parent.gameObject.SetActive(true);
                Instance.UsePassword.isOn = true;
                Instance.UsePassword.gameObject.SetActive(false);
                configurePrivateKeyField("", "", false, "");
                Instance.KeyStoreDropbox.gameObject.SetActive(true);
                break;
            case AccountMenuState.PasswordRequired:
                Instance.PrivateKeyInput.transform.parent.gameObject.SetActive(false);
                Instance.PasswordField.transform.parent.gameObject.SetActive(true);
                Instance.UsePassword.gameObject.SetActive(false);
                Instance.KeyStoreDropbox.gameObject.SetActive(false);
                Instance.AccountUnlockText[0].text = "Welcome back!";
                Instance.AccountUnlockText[1].text = "Your private key has been encrypted with AES and stored in AppData.\n To decrypt your private key, we need your password!";
                Instance.PasswordField.placeholder.GetComponent<Text>().text = "password123";
                break;
            case AccountMenuState.ShowAccountInfo:
                Instance.AccountInfoText[0].text = NetworkSettings.address;
                Instance.AccountInfoText[1].text = "0 MTX";
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
        yield break;
    }

    public void toggleUsePassword()
    {
        if (Instance.UsePassword.isOn)
        {
            if (Instance.State == AccountMenuState.PrivateKeyInput)
            {
                SetState(AccountMenuState.PrivateKeyPasswordInput);
            }
            else if (Instance.State == AccountMenuState.MnemonicInput)
            {
                SetState(AccountMenuState.MnemonicPasswordInput);
            }
        }
        else
        {
            if (Instance.State == AccountMenuState.PrivateKeyPasswordInput)
            {
                SetState(AccountMenuState.PrivateKeyInput);
            }
            else if (Instance.State == AccountMenuState.MnemonicPasswordInput)
            {
                SetState(AccountMenuState.MnemonicInput);
            }
        }
    }

    public void TakeInput()
    {
        NetworkSettings.declinedAccountUnlock = false;

        switch (Instance.State)
        {
            case AccountMenuState.PasswordRequired:
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
            Instance.PrivateKeyInput.text = "";
            Instance.PasswordField.text = "";
        }

        Instance.AccountUnlockText[2].gameObject.SetActive(false);
    }

    public enum InputType
    {
        Password,
        MnemonicPhrase,
        PrivateKey,
        Keystore
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

    //// returns success
    public static bool TrySetPrivateKeyAndCypher()
    {
        string password = Instance.PasswordField.text;


        if (Instance.State == AccountMenuState.PrivateKeyInput || Instance.State == AccountMenuState.PrivateKeyPasswordInput)
        {
            string privateKey = Instance.PrivateKeyInput.text;
            bool usedPassword = Instance.UsePassword.isOn;

            if (!Instance.PrivateKeyInput.GetComponent<InputValidator>().isValid)
            {
                Instance.AccountUnlockText[2].text = "Invalid private key";
                Instance.AccountUnlockText[2].gameObject.SetActive(true);
                return false;
            }

            if (usedPassword && !Instance.PasswordField.GetComponent<InputValidator>().isValid)
            {
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
        else if (Instance.State == AccountMenuState.MnemonicInput || Instance.State == AccountMenuState.MnemonicPasswordInput)
        {
            string mnemonic = Instance.PrivateKeyInput.text;
            // Get private key from mnemonic
            var wallet = new Nethereum.HdWallet.Wallet(mnemonic, password);
            byte[] sK = wallet.GetPrivateKey(0);

            var zeroExPrivateKey = "0x" + System.BitConverter.ToString(sK).Replace("-", "");
            Debug.Log("this is the private key: " + zeroExPrivateKey);
        }
        else if (Instance.State == AccountMenuState.KeyStoreInput)
        {
            string mnemonic = Instance.PrivateKeyInput.text;
            var service = new Nethereum.KeyStore.KeyStoreService();
            var privateKey = service.DecryptKeyStoreFromJson(password, mnemonic);
            Debug.Log("private key: 0x" + System.BitConverter.ToString(Encoding.Unicode.GetBytes(mnemonic.ToCharArray())).Replace("-", ""));
        }

        return false;
    }

    //// returns success
    public static bool TryUnlockAccount()
    {
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

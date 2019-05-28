using Crosstales.FB;
using Matryx;
using Nanome.Core;
using System.Collections;
using System.Numerics;
using System.Text;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityEngine.EventSystems;
using UnityEngine.UI;

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
    InputField PasswordInput;
    [SerializeField]
    InputField ConfirmPasswordInput;
    [SerializeField]
    Button ImportButton;
    [SerializeField]
    Toggle UsePassword;
    [SerializeField]
    public Text ErrorMessage;
    

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

    static string keystorePath = "";

    public void OnEnable()
    {
        if (Instance == null)
        {
            Instance = this;
            OVRManager.HMDMounted += ReenterVR;
        }

        ImportButton.onClick.AddListener(loadKeystore);
        PrivateKeyInput.onValueChanged.AddListener(onAccountFieldEdited);
        PasswordInput.onValueChanged.AddListener(onAccountFieldEdited);

        var cypher = Config.getString("cypher", "");
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

    public void Update()
    {
        if (PrivateKeyInput.text.Length == 0 && Instance.State == AccountMenuState.KeyStoreInput)
        {
            Instance.ImportButton.gameObject.SetActive(true);
        }
    }

    public static void loadKeystore()
    {
        keystorePath = FileBrowser.OpenSingleFile();
        Instance.PrivateKeyInput.text = keystorePath;
        Instance.ImportButton.gameObject.SetActive(false);

        if (!validateKeystore())
        {
            throw new System.Exception("Keystore file invalid");
        }
    }

    public static bool validateKeystore()
    {
        return true;
    }

    public static void onAccountFieldEdited(string fieldText)
    {
        MatryxAccountMenu.Instance.ErrorMessage.gameObject.SetActive(true);
        MatryxAccountMenu.Instance.ErrorMessage.text = "";
    }

    public void configurePrivateKeyField(string title, string placeholderText, bool active, string validationType)
    {
        Instance.PrivateKeyInput.transform.parent.GetChild(0).GetComponent<Text>().text = title;
        Instance.PrivateKeyInput.placeholder.GetComponent<Text>().text = placeholderText;
        Instance.PrivateKeyInput.transform.parent.gameObject.SetActive(active);
        Instance.PrivateKeyInput.GetComponent<InputValidator>().setValidationType(validationType);
    }

    public void SetState(AccountMenuState state)
    {
        Instance.gameObject.SetActive(true);
        Instance.State = state;
        Instance.accountImportType.gameObject.SetActive(true);
        ImportButton.gameObject.SetActive(false);

        switch (state)
        {
            case AccountMenuState.PrivateKeyInput:
                Instance.PasswordInput.transform.parent.gameObject.SetActive(false);
                Instance.ConfirmPasswordInput.transform.parent.gameObject.SetActive(false);
                Instance.UsePassword.transform.parent.gameObject.SetActive(true);
                Instance.UsePassword.isOn = false;
                Instance.UsePassword.gameObject.SetActive(true);
                configurePrivateKeyField("Private Key", "0x...", true, InputValidator.INPUT_VALIDATION_PRIVATE_KEY);
                break;
            case AccountMenuState.PrivateKeyPasswordInput:
                Instance.PasswordInput.transform.parent.gameObject.SetActive(true);
                Instance.ConfirmPasswordInput.transform.parent.gameObject.SetActive(true);
                Instance.UsePassword.transform.parent.gameObject.SetActive(true);
                Instance.UsePassword.isOn = true;
                Instance.UsePassword.gameObject.SetActive(true);
                configurePrivateKeyField("Private Key", "0x...", true, InputValidator.INPUT_VALIDATION_PRIVATE_KEY);
                break;
            case AccountMenuState.MnemonicInput:
                Instance.PasswordInput.transform.parent.gameObject.SetActive(false);
                Instance.ConfirmPasswordInput.transform.parent.gameObject.SetActive(false);
                Instance.UsePassword.transform.parent.gameObject.SetActive(true);
                Instance.UsePassword.isOn = false;
                Instance.UsePassword.gameObject.SetActive(true);
                configurePrivateKeyField("Mnemonic Phrase", "Your mnemonic seed", true, InputValidator.INPUT_VALIDATION_MNEMONIC_PHRASE);
                break;
            case AccountMenuState.MnemonicPasswordInput:
                Instance.PasswordInput.transform.parent.gameObject.SetActive(true);
                Instance.ConfirmPasswordInput.transform.parent.gameObject.SetActive(true);
                Instance.ConfirmPasswordInput.transform.parent.gameObject.SetActive(true);
                Instance.UsePassword.transform.parent.gameObject.SetActive(true);
                Instance.UsePassword.isOn = true;
                Instance.UsePassword.gameObject.SetActive(true);
                configurePrivateKeyField("Mnemonic Phrase", "Your mnemonic seed", true, InputValidator.INPUT_VALIDATION_MNEMONIC_PHRASE);
                break;
            case AccountMenuState.KeyStoreInput:
                ImportButton.gameObject.SetActive(true);
                Instance.PasswordInput.transform.parent.gameObject.SetActive(true);
                Instance.ConfirmPasswordInput.transform.parent.gameObject.SetActive(true);
                Instance.UsePassword.isOn = true;
                Instance.UsePassword.gameObject.SetActive(false);
                configurePrivateKeyField("Keystore file", "path/to/keystore", true, InputValidator.INPUT_VALIDATION_KEYSTORE_FILE);
                break;
            case AccountMenuState.PasswordRequired:
                Instance.accountImportType.gameObject.SetActive(false);
                Instance.PrivateKeyInput.transform.parent.gameObject.SetActive(false);
                Instance.PasswordInput.transform.parent.gameObject.SetActive(true);
                Instance.ConfirmPasswordInput.transform.parent.gameObject.SetActive(false);
                Instance.UsePassword.gameObject.SetActive(false);
                Instance.AccountUnlockText[0].text = "Welcome back!";
                Instance.AccountUnlockText[1].text = "Your private key has been encrypted with AES and stored in AppData.\n To decrypt your private key, we need your password!";
                Instance.PasswordInput.placeholder.GetComponent<Text>().text = "password123";
                break;
            case AccountMenuState.ShowAccountInfo:
                Instance.AccountInfoText[0].text = NetworkSettings.currentAddress;
                StartCoroutine(updateMTXBalance());
                Instance.StartCoroutine(Instance.waitAndThenReenterVR());
                break;
            default:
                break;
        }

        bool isAccountInfo = state == AccountMenuState.ShowAccountInfo;
        Instance.AccountUnlockMenu.SetActive(!isAccountInfo);
        Instance.AccountInfoMenu.SetActive(isAccountInfo);
    }

    public IEnumerator updateMTXBalance()
    {
        var tokenBalanceCoroutine = new Utils.CoroutineWithData<BigInteger>(MatryxCortex.Instance, MatryxToken.balanceOf(NetworkSettings.currentAddress));
        yield return tokenBalanceCoroutine;
        var balance = tokenBalanceCoroutine.result / new BigInteger(1e18);
        Instance.AccountInfoText[1].text = balance + " MTX";
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
            Instance.PasswordInput.text = "";
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

    // returns success
    public static bool TrySetPrivateKeyAndCypher()
    {
        string password = Instance.PasswordInput.text;
        string confirmPassword = Instance.ConfirmPasswordInput.text;
        bool usedPassword = Instance.UsePassword.isOn;
        string cypher = "";
        string cypherType = "";

        if (password != confirmPassword)
        {
            Instance.ErrorMessage.gameObject.SetActive(true);
            Instance.ErrorMessage.text = "Passwords do not match";
            return false;
        }

        if (Instance.State == AccountMenuState.PrivateKeyInput || Instance.State == AccountMenuState.PrivateKeyPasswordInput)
        {
            string privateKey = Instance.PrivateKeyInput.text;
            

            if (!Instance.PrivateKeyInput.GetComponent<InputValidator>().isValid)
            {
                Instance.AccountUnlockText[2].text = "Invalid private key";
                Instance.AccountUnlockText[2].gameObject.SetActive(true);
                return false;
            }

            if (usedPassword && !Instance.PasswordInput.GetComponent<InputValidator>().isValid)
            {
                Instance.AccountUnlockText[2].text = "Invalid password";
                Instance.AccountUnlockText[2].gameObject.SetActive(true);
                return false;
            }

            try
            {
                NetworkSettings.importKey(privateKey, true);
            }
            catch (System.Exception e)
            {
                Instance.ErrorMessage.gameObject.SetActive(true);
                Instance.ErrorMessage.text = "" + e;
                return false;
            }

            // Encrypt private key
            cypher = Crypto.Encrypt(privateKey, usedPassword ? password : "");
            cypherType = "private key";
        }
        else if (Instance.State == AccountMenuState.MnemonicInput || Instance.State == AccountMenuState.MnemonicPasswordInput)
        {
            string mnemonic = Instance.PrivateKeyInput.text;
            // Store mnemonic in NetworkSettings for transactions
            try
            {
                NetworkSettings.importWallet(mnemonic, "");
            }
            catch (System.Exception e)
            {
                Instance.ErrorMessage.gameObject.SetActive(true);
                Instance.ErrorMessage.text = "" + e;
                return false;
            }

            // Encrypt mnemonic
            cypher = Crypto.Encrypt(mnemonic, usedPassword ? password : "");
            cypherType = "mnemonic";
        }
        else if (Instance.State == AccountMenuState.KeyStoreInput)
        {
            string keystore = File.readFileContent(keystorePath);

            try
            {
                NetworkSettings.importKeystore(keystore, "");
            }
            catch (System.Exception e)
            {
                Instance.ErrorMessage.gameObject.SetActive(true);
                Instance.ErrorMessage.text = "Invalid keystore file";
                return false;
            }
            
            // Encrypt keystore
            cypher = Crypto.Encrypt(keystorePath, usedPassword ? password : "");
            cypherType = "keystore";
        }

        // Store cypher and related info
        Config.setString("cypher", cypher, true, "storage");
        Config.setString("cypherType", cypherType, true, "storage");
        Config.setString("address", NetworkSettings.currentAddress);
        Config.setBool("usedPassword", usedPassword, true, "storage");

        TournamentsMenu.SetState(TournamentsMenu.TournamentMenuState.Unlocked);
        return true;
    }

    public static bool UnlockAccount()
    {
        string cypher = Config.getString("cypher", "");
        if (cypher.Equals("")) { return false; }
        string cypherType = Config.getString("cypherType", "");
        bool usedPassword = Config.getBool("usedPassword", "0");
        string plainText = Crypto.Decrypt(cypher);

        if(usedPassword)
        {
            return false;
        }

        NetworkSettings.declinedAccountUnlock = false;
        switch (cypherType)
        {
            case "private key":
                if (plainText.Substring(0, 2) != "0x")
                {
                    return false;
                }
                return NetworkSettings.importKey(plainText);
            case "mnemonic":
                try
                {
                    return NetworkSettings.importWallet(plainText);
                }
                catch (System.Exception e)
                {
                    return false;
                }
            case "keystore":
                try
                {
                    return NetworkSettings.importKeystore(plainText, "");
                }
                catch (System.Exception e)
                {
                    return false;
                }
        }

        return true;
    }

    // returns success
    public static bool TryUnlockAccount()
    {
        var password = Instance.PasswordInput.text;
        string cypher = Config.getString("cypher", "");
        string cypherType = Config.getString("cypherType", "");
        string plainText = Crypto.Decrypt(cypher, password);

        NetworkSettings.declinedAccountUnlock = false;
        switch (cypherType)
        {
            case "private key":
                if (plainText.Substring(0, 2) != "0x")
                {
                    Instance.PasswordInput.GetComponent<Image>().color = new Color(1f, 181f / 255f, 181f / 255f);
                    Instance.AccountUnlockText[2].text = "Invalid password";
                    Instance.AccountUnlockText[2].gameObject.SetActive(true);
                    return false;
                }
                NetworkSettings.importKey(plainText);
                break;
            case "mnemonic":
                try
                {
                    NetworkSettings.importWallet(plainText, "");
                }
                catch (System.Exception e)
                {
                    Instance.ErrorMessage.gameObject.SetActive(true);
                    Instance.ErrorMessage.text = "Invalid password";
                    return false;
                }
                
                break;
            case "keystore":
                try
                {
                    NetworkSettings.importKeystore(plainText, "");
                }
                catch (System.Exception e)
                {
                    Instance.ErrorMessage.gameObject.SetActive(true);
                    Instance.ErrorMessage.text = "Invalid password";
                    return false;
                }
                break;
        }

        TournamentsMenu.SetState(TournamentsMenu.TournamentMenuState.Unlocked);

        return true;
    }
}

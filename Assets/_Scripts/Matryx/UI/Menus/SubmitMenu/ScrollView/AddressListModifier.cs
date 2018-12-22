using System.Collections;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityEngine.UI;

public class AddressListModifier : MonoBehaviour {

    [SerializeField]
    GameObject contentHolder;
    [SerializeField]
    InputField inputField;
    [SerializeField]
    Image inputBackgroundImage;
    [SerializeField]
    GameObject addressCellPrefab;

    Color defaultColor;

    HashSet<string> addresses = new HashSet<string>();

    public List<string> GetAddressList()
    {
        return addresses.ToList<string>();
    }

    private void Start()
    {
        defaultColor = inputBackgroundImage.color;
    }

    public void InsertFromInputField()
    {
        List<string> addresses = GetAddressesFromInput();
        foreach (string address in addresses)
        {
            Insert(address);
        }

        inputField.text = "";
    }
    
    /// <summary>
    /// Returns true if the input field is valid.
    /// </summary>
    /// <returns></returns>
    public List<string> GetAddressesFromInput()
    {
        string address = inputField.text;

        Regex rgx = new Regex(@"(0x)?[0-9A-Fa-f]{40}");

        MatchCollection matchList = rgx.Matches(address);
        var list = matchList.Cast<Match>().Select(match => match.Value).ToList();

        if(list.Count == 0)
        {
            Color redTintDefaultColor = new Color(defaultColor.r + 0.2f, defaultColor.g / 3f, defaultColor.b / 3f);
            inputBackgroundImage.color = redTintDefaultColor;

            return new List<string>();
        }
        else
        {
            inputBackgroundImage.color = defaultColor;

            return list;
        }
    }
	
    /// <summary>
    /// Inserts an address cell into the address view.
    /// </summary>
    /// <param name="address"> The address to insert into the view </param>
    /// <returns> Whether or not a new cell was inserted. </returns>
    public bool Insert(string address)
    {
        bool addressWasAdded = addresses.Add(address);

        if(addressWasAdded)
        {
            // Create a new address cell
            GameObject addressCell = Instantiate(addressCellPrefab) as GameObject;
            addressCell.SetActive(true);
            // Set its name
            SimpleButton button = addressCell.GetComponent<SimpleButton>();
            button.SetName(address);
            button.addressModifier = this;
            // Add it to the content view
            addressCell.transform.SetParent(contentHolder.transform);
            addressCell.transform.localScale = Vector3.one;
        }

        // Return whether or not the address was added
        return addressWasAdded;
    }

    /// <summary>
    /// Removes an address cell from the address view.
    /// </summary>
    /// <param name="address"> The address to remove from the view </param>
    /// <returns> Whether or not a cell was removed. </returns>
    public bool Remove(string address)
    {
        // Try to remove it from our local set
        bool addressWasRemoved = addresses.Remove(address);

        // If we did remove it, delete the gameobject
        if(addressWasRemoved)
        {
            Transform addressCellTransform = contentHolder.transform.Find(address);
            GameObject addressCell = addressCellTransform.gameObject;

            Destroy(addressCell);
        }
        
        // Return whether or not the address was removed
        return addressWasRemoved;
    }

    /// <summary>
    /// Removes all address cells from the address view.
    /// </summary>
    /// <returns> Index of the first cell that could not be removed. </returns>
    public int RemoveAll()
    {
        var values = GetAddressList();
        int i;

        for(i = 0; i < values.Count; i++)
        {
            if(!Remove(values[i]))
            {
                break;
            }
        }

        return i;
    }

}
